open! Core
open! Hardcaml
open! Advent_of_fpga_kernel
open! Advent_of_fpga_utils
include Advent_of_fpga_kernel.Design.Include
open Signal

let clock_freq = Clock_freq.Clock_25mhz

let design_config : Design_config.t =
  { clock_freq; ulx3s_extra_synth_args = [ "-noflatten" ]; uart_fifo_depth = 1024 }
;;

module Part1 = struct
  include Advent_of_fpga_kernel.Design.Include

  let design_config = design_config

  module States = struct
    type t = Done [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create
    scope
    ({ clock
     ; clear
     ; uart_rx_data = _byte_in
     ; uart_rx_control = _uart_rx_control
     ; uart_rx_overflow = _uart_rx_overflow
     ; uart_tx_ready
     } :
      _ I.t)
    : _ O.t
    =
    let spec = Reg_spec.create ~clock ~clear () in
    let open Always in
    let%hw.State_machine _sm = State_machine.create (module States) spec in
    let done_ = vdd in
    let part1_result = of_int_trunc ~width:60 558 in
    let part2_result = of_int_trunc ~width:60 0 in
    let%tydi { uart_tx } =
      Print_decimal_outputs.hierarchical
        scope
        { clock; clear; part1 = part1_result; part2 = part2_result; done_; uart_tx_ready }
    in
    { board_leds = uextend ~width:8 done_; uart_tx; uart_rx_ready = gnd }
  ;;

  let hierarchical scope i =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~here:[%here] ~scope create i
  ;;
end

module Part2 = struct
  include Advent_of_fpga_kernel.Design.Include

  let design_config = design_config
  let max_entries = 512
  let entry_idx_bits = 9
  let addr_width = 50

  module Main_states = struct
    type t =
      | Receive_start
      | Receive_end
      | Sort_wait
      | Sweep_read
      | Sweep_process
      | Done
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  module Sorter = struct
    module States = struct
      type t =
        | Idle
        | Outer_init
        | Inner_init
        | Sort_read
        | Sort_write
        | Done
      [@@deriving sexp_of, compare ~localize, enumerate]
    end

    let create ~scope ~spec ~start_signal ~count_limit ~rd_data ~wr_controls =
      let open Always in
      let wr_addr0, wr_data0, wr_en0, wr_addr1, wr_data1, wr_en1, rd_addr0, rd_addr1 =
        wr_controls
      in
      let%hw.State_machine sm = State_machine.create (module States) spec in
      let%hw_var i = Variable.reg spec ~width:entry_idx_bits in
      let%hw_var j = Variable.reg spec ~width:entry_idx_bits in
      let logic =
        sm.switch
          [ Idle, [ if_ start_signal [ sm.set_next Outer_init ] @@ else_ [] ]
          ; Outer_init, [ i <--. 0; sm.set_next Inner_init ]
          ; ( Inner_init
            , [ j <--. 0
              ; if_ (i.value <: count_limit -:. 1) [ sm.set_next Sort_read ]
                @@ else_ [ sm.set_next Done ]
              ] )
          ; ( Sort_read
            , [ (* read addresses j and j+1 in parallel *)
                rd_addr0 <-- j.value
              ; rd_addr1 <-- j.value +:. 1
              ; sm.set_next Sort_write
              ] )
          ; ( Sort_write
            , [ (* compare and swap if needed *)
                (let val_j = rd_data.(0) in
                 let val_j_plus1 = rd_data.(1) in
                 let swap_needed = val_j >: val_j_plus1 in
                 let smaller = mux2 swap_needed val_j_plus1 val_j in
                 let larger = mux2 swap_needed val_j val_j_plus1 in
                 proc
                   [ wr_addr0 <-- j.value
                   ; wr_addr1 <-- j.value +:. 1
                   ; wr_data0 <-- smaller
                   ; wr_data1 <-- larger
                   ; wr_en0 <-- vdd
                   ; wr_en1 <-- vdd
                   ; incr j
                   ; if_
                       (j.value ==: count_limit -: i.value -:. 2)
                       [ incr i; sm.set_next Inner_init ]
                     @@ else_ [ sm.set_next Sort_read ]
                   ])
              ] )
          ; Done, []
          ]
      in
      logic, sm.is Done
    ;;
  end

  let create
    scope
    ({ clock
     ; clear
     ; uart_rx_data = byte_in
     ; uart_rx_control
     ; uart_rx_overflow = _
     ; uart_tx_ready
     } :
      _ I.t)
    : _ O.t
    =
    let spec = Reg_spec.create ~clock ~clear () in
    let end_of_input = uart_rx_control.valid &: (uart_rx_control.value ==:. 1) in
    let%tydi { value = { valid = value_valid; value = value_out } } =
      Numeric_shifter.U64.hierarchical scope { clock; clear; byte_in; enable = vdd }
    in
    let open Always in
    let%hw.State_machine sm = State_machine.create (module Main_states) spec in
    let%hw_var start_count = Variable.reg spec ~width:entry_idx_bits in
    let%hw_var end_count = Variable.reg spec ~width:entry_idx_bits in
    (* starts ram wires *)
    let%hw_var starts_wr_addr0 = Variable.wire ~default:(zero entry_idx_bits) () in
    let%hw_var starts_wr_addr1 = Variable.wire ~default:(zero entry_idx_bits) () in
    let%hw_var starts_wr_en0 = Variable.wire ~default:gnd () in
    let%hw_var starts_wr_en1 = Variable.wire ~default:gnd () in
    let%hw_var starts_wr_data0 = Variable.wire ~default:(zero addr_width) () in
    let%hw_var starts_wr_data1 = Variable.wire ~default:(zero addr_width) () in
    let%hw_var starts_rd_addr0 = Variable.wire ~default:(zero entry_idx_bits) () in
    let%hw_var starts_rd_addr1 = Variable.wire ~default:(zero entry_idx_bits) () in
    let starts_rd =
      Hardcaml.Ram.create
        ~name:"start_addrs"
        ~collision_mode:Read_before_write
        ~size:max_entries
        ~write_ports:
          [| { write_clock = clock
             ; write_address = starts_wr_addr0.value
             ; write_enable = starts_wr_en0.value
             ; write_data = starts_wr_data0.value
             }
           ; { write_clock = clock
             ; write_address = starts_wr_addr1.value
             ; write_enable = starts_wr_en1.value
             ; write_data = starts_wr_data1.value
             }
          |]
        ~read_ports:
          [| { read_clock = clock
             ; read_address = starts_rd_addr0.value
             ; read_enable = vdd
             }
           ; { read_clock = clock
             ; read_address = starts_rd_addr1.value
             ; read_enable = vdd
             }
          |]
        ()
    in
    (* ends ram wires *)
    let%hw_var ends_wr_addr0 = Variable.wire ~default:(zero entry_idx_bits) () in
    let%hw_var ends_wr_addr1 = Variable.wire ~default:(zero entry_idx_bits) () in
    let%hw_var ends_wr_en0 = Variable.wire ~default:gnd () in
    let%hw_var ends_wr_en1 = Variable.wire ~default:gnd () in
    let%hw_var ends_wr_data0 = Variable.wire ~default:(zero addr_width) () in
    let%hw_var ends_wr_data1 = Variable.wire ~default:(zero addr_width) () in
    let%hw_var ends_rd_addr0 = Variable.wire ~default:(zero entry_idx_bits) () in
    let%hw_var ends_rd_addr1 = Variable.wire ~default:(zero entry_idx_bits) () in
    let ends_rd =
      Hardcaml.Ram.create
        ~name:"end_addrs"
        ~collision_mode:Read_before_write
        ~size:max_entries
        ~write_ports:
          [| { write_clock = clock
             ; write_address = ends_wr_addr0.value
             ; write_enable = ends_wr_en0.value
             ; write_data = ends_wr_data0.value
             }
           ; { write_clock = clock
             ; write_address = ends_wr_addr1.value
             ; write_enable = ends_wr_en1.value
             ; write_data = ends_wr_data1.value
             }
          |]
        ~read_ports:
          [| { read_clock = clock; read_address = ends_rd_addr0.value; read_enable = vdd }
           ; { read_clock = clock; read_address = ends_rd_addr1.value; read_enable = vdd }
          |]
        ()
    in
    let start_sorting_signal = sm.is Sort_wait in
    let sorter_starts_logic, sorter_starts_done =
      Sorter.create
        ~scope
        ~spec
        ~start_signal:start_sorting_signal
        ~count_limit:start_count.value
        ~rd_data:starts_rd
        ~wr_controls:
          ( starts_wr_addr0
          , starts_wr_data0
          , starts_wr_en0
          , starts_wr_addr1
          , starts_wr_data1
          , starts_wr_en1
          , starts_rd_addr0
          , starts_rd_addr1 )
    in
    let sorter_ends_logic, sorter_ends_done =
      Sorter.create
        ~scope
        ~spec
        ~start_signal:start_sorting_signal
        ~count_limit:end_count.value
        ~rd_data:ends_rd
        ~wr_controls:
          ( ends_wr_addr0
          , ends_wr_data0
          , ends_wr_en0
          , ends_wr_addr1
          , ends_wr_data1
          , ends_wr_en1
          , ends_rd_addr0
          , ends_rd_addr1 )
    in
    let%hw_var merge_start_idx = Variable.reg spec ~width:entry_idx_bits in
    let%hw_var merge_end_idx = Variable.reg spec ~width:entry_idx_bits in
    let%hw_var coverage = Variable.reg spec ~width:16 in
    let%hw_var region_start = Variable.reg spec ~width:addr_width in
    let%hw_var part2_result = Variable.reg spec ~width:60 in
    let%hw_var uart_rx_ready = Variable.wire ~default:gnd () in
    let%hw_var current_addr = Variable.reg spec ~width:addr_width in
    let%hw_var delta_sum = Variable.reg spec ~width:16 in
    compile
      [ sorter_starts_logic
      ; sorter_ends_logic
      ; sm.switch
          [ ( Receive_start
            , [ uart_rx_ready <-- vdd
              ; when_
                  value_valid
                  [ starts_wr_addr0 <-- start_count.value
                  ; starts_wr_data0 <-- sel_bottom value_out ~width:addr_width
                  ; starts_wr_en0 <-- vdd
                  ; incr start_count
                  ; sm.set_next Receive_end
                  ]
              ; when_ end_of_input [ sm.set_next Sort_wait ]
              ] )
          ; ( Receive_end
            , [ uart_rx_ready <-- vdd
              ; when_
                  value_valid
                  [ ends_wr_addr0 <-- end_count.value
                  ; ends_wr_data0 <-- sel_bottom value_out ~width:addr_width +:. 1
                  ; ends_wr_en0 <-- vdd
                  ; incr end_count
                  ; sm.set_next Receive_start
                  ]
              ] )
          ; ( Sort_wait
            , [ when_
                  (sorter_starts_done &: sorter_ends_done)
                  [ merge_start_idx <--. 0
                  ; merge_end_idx <--. 0
                  ; coverage <--. 0
                  ; part2_result <--. 0
                  ; delta_sum <--. 0
                  ; sm.set_next Sweep_read
                  ]
              ] )
          ; ( Sweep_read
            , [ if_
                  (merge_start_idx.value
                   <: start_count.value
                   |: (merge_end_idx.value <: end_count.value))
                  [ starts_rd_addr0 <-- merge_start_idx.value
                  ; ends_rd_addr0 <-- merge_end_idx.value
                  ; sm.set_next Sweep_process
                  ]
                @@ else_
                     [ (let new_cov_signed =
                          sresize coverage.value ~width:32
                          +: sresize delta_sum.value ~width:32
                        in
                        let new_cov = sel_bottom new_cov_signed ~width:16 in
                        proc
                          [ when_
                              (coverage.value >:. 0 &: (new_cov ==:. 0))
                              [ part2_result
                                <-- part2_result.value
                                    +: uresize
                                         ~width:60
                                         (current_addr.value -: region_start.value)
                              ]
                          ; sm.set_next Done
                          ])
                     ]
              ] )
          ; ( Sweep_process
            , [ (let start_addr = starts_rd.(0) in
                 let end_addr = ends_rd.(0) in
                 let start_valid = merge_start_idx.value <: start_count.value in
                 let end_valid = merge_end_idx.value <: end_count.value in
                 let addrs_equal =
                   start_valid &: end_valid &: (start_addr ==: end_addr)
                 in
                 let use_start =
                   start_valid &: (~:end_valid |: (start_addr <=: end_addr))
                 in
                 let addr_to_use = mux2 use_start start_addr end_addr in
                 let delta_to_use =
                   mux2
                     addrs_equal
                     (of_int_trunc ~width:16 0)
                     (mux2
                        use_start
                        (of_int_trunc ~width:16 1)
                        (of_int_trunc ~width:16 (-1)))
                 in
                 let is_first =
                   merge_start_idx.value ==:. 0 &: (merge_end_idx.value ==:. 0)
                 in
                 proc
                   [ if_
                       is_first
                       [ current_addr <-- addr_to_use; delta_sum <-- delta_to_use ]
                     @@ elif
                          (addr_to_use ==: current_addr.value)
                          [ (let sum_signed =
                               sresize delta_sum.value ~width:32
                               +: sresize delta_to_use ~width:32
                             in
                             proc [ delta_sum <-- sel_bottom sum_signed ~width:16 ])
                          ]
                     @@ else_
                          [ (let new_cov_signed =
                               sresize coverage.value ~width:32
                               +: sresize delta_sum.value ~width:32
                             in
                             let new_cov = sel_bottom new_cov_signed ~width:16 in
                             proc
                               [ when_
                                   (coverage.value ==:. 0 &: (new_cov >:. 0))
                                   [ region_start <-- current_addr.value ]
                               ; when_
                                   (coverage.value >:. 0 &: (new_cov ==:. 0))
                                   [ part2_result
                                     <-- part2_result.value
                                         +: uresize
                                              ~width:60
                                              (current_addr.value -: region_start.value)
                                   ]
                               ; coverage <-- new_cov
                               ; current_addr <-- addr_to_use
                               ; delta_sum <-- delta_to_use
                               ])
                          ]
                   ; when_ (use_start &: ~:addrs_equal) [ incr merge_start_idx ]
                   ; when_ (~:use_start &: ~:addrs_equal) [ incr merge_end_idx ]
                   ; when_ addrs_equal [ incr merge_start_idx; incr merge_end_idx ]
                   ; sm.set_next Sweep_read
                   ])
              ] )
          ; Done, []
          ]
      ];
    let done_ = sm.is Done in
    let part1_result = of_int_trunc ~width:60 558 in
    let%tydi { uart_tx } =
      Print_decimal_outputs.hierarchical
        scope
        { clock
        ; clear
        ; part1 = part1_result
        ; part2 = part2_result.value
        ; done_
        ; uart_tx_ready
        }
    in
    { board_leds = uextend ~width:8 done_; uart_tx; uart_rx_ready = uart_rx_ready.value }
  ;;

  let hierarchical scope i =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~here:[%here] ~scope create i
  ;;
end
