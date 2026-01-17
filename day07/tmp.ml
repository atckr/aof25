open! Core
open! Hardcaml
open! Advent_of_fpga_kernel
open! Advent_of_fpga_utils
include Advent_of_fpga_kernel.Design.Include
open Signal

let clock_freq = Clock_freq.Clock_25mhz

let design_config =
  { Design_config.default with clock_freq; ulx3s_extra_synth_args = [ "-noflatten" ] }
;;

let max_width = 256
let width_bits = 8

(* Character encoding from parser: 0='.' 1='S' 2='^' *)
let char_is_dot c = c ==:. 0
let char_is_start c = c ==:. 1
let char_is_caret c = c ==:. 2

module States = struct
  type t =
    | Read_width
    | Init_state_ram
    | Read_line
    | Process_read
    | Process_execute
    | Spread_read (* Reusable: read char[target] and state[target] *)
    | Spread_write (* Reusable: if char=='.': state[target] += beam *)
    | Process_next
    | Sum_read
    | Sum_accumulate
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
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
  let open Always in
  (* Line RAM - stores 2-bit encoded characters for current line *)
  let%hw_var line_wr_addr = Variable.wire ~default:(zero width_bits) () in
  let%hw_var line_wr_en = Variable.wire ~default:gnd () in
  let%hw_var line_wr_data = Variable.wire ~default:(zero 2) () in
  let%hw_var line_rd_addr = Variable.reg spec ~width:width_bits in
  let line_ram =
    Ram.create
      ~name:"line_ram"
      ~collision_mode:Read_before_write
      ~size:max_width
      ~write_ports:
        [| { write_clock = clock
           ; write_address = line_wr_addr.value
           ; write_enable = line_wr_en.value
           ; write_data = line_wr_data.value
           }
        |]
      ~read_ports:
        [| { read_clock = clock; read_address = line_rd_addr.value; read_enable = vdd } |]
      ()
  in
  let line_rd_data = line_ram.(0) in
  (* State RAM - stores 64-bit state values for each column *)
  let%hw_var state_wr_addr = Variable.wire ~default:(zero width_bits) () in
  let%hw_var state_wr_en = Variable.wire ~default:gnd () in
  let%hw_var state_wr_data = Variable.wire ~default:(zero 64) () in
  let%hw_var state_rd_addr = Variable.reg spec ~width:width_bits in
  let state_ram =
    Ram.create
      ~name:"state_ram"
      ~collision_mode:Read_before_write
      ~size:max_width
      ~write_ports:
        [| { write_clock = clock
           ; write_address = state_wr_addr.value
           ; write_enable = state_wr_en.value
           ; write_data = state_wr_data.value
           }
        |]
      ~read_ports:
        [| { read_clock = clock; read_address = state_rd_addr.value; read_enable = vdd }
        |]
      ()
  in
  let state_rd_data = state_ram.(0) in
  let%hw.State_machine sm = State_machine.create (module States) spec in
  (* Registers *)
  let%hw_var line_len = Variable.reg spec ~width:width_bits in
  let%hw_var char_idx = Variable.reg spec ~width:width_bits in
  let%hw_var col_idx = Variable.reg spec ~width:width_bits in
  let%hw_var init_idx = Variable.reg spec ~width:width_bits in
  let%hw_var result = Variable.reg spec ~width:64 in
  let%hw_var last_line = Variable.reg spec ~width:1 in
  let%hw_var uart_rx_ready = Variable.wire ~default:gnd () in
  (* For spread: reusable update_beam logic *)
  let%hw_var saved_beam = Variable.reg spec ~width:64 in
  let%hw_var spread_target = Variable.reg spec ~width:width_bits in
  let%hw_var spread_right_pending = Variable.reg spec ~width:1 in
  (* For part 2 *)
  let%hw_var part2_sum = Variable.reg spec ~width:64 in
  let%hw_var sum_idx = Variable.reg spec ~width:width_bits in
  compile
    [ sm.switch
        [ ( Read_width
          , [ uart_rx_ready <-- vdd
            ; when_
                byte_in.valid
                [ line_len <-- sel_bottom byte_in.value ~width:width_bits
                ; init_idx <--. 0
                ; sm.set_next Init_state_ram
                ]
            ] )
        ; ( Init_state_ram
          , [ (* Python: state = [0] * m *)
              state_wr_addr <-- init_idx.value
            ; state_wr_data <--. 0
            ; state_wr_en <-- vdd
            ; if_
                (init_idx.value ==: line_len.value -:. 1)
                [ char_idx <--. 0; sm.set_next Read_line ]
              @@ else_ [ incr init_idx ]
            ] )
        ; ( Read_line
          , [ uart_rx_ready <-- vdd
            ; when_
                byte_in.valid
                [ line_wr_addr <-- char_idx.value
                ; line_wr_data <-- sel_bottom byte_in.value ~width:2
                ; line_wr_en <-- vdd
                ; if_
                    (char_idx.value ==: line_len.value -:. 1)
                    [ char_idx <--. 0
                    ; col_idx <--. 0
                    ; line_rd_addr <--. 0
                    ; state_rd_addr <--. 0
                    ; sm.set_next Process_read
                    ]
                  @@ else_ [ incr char_idx ]
                ]
            ; when_
                (uart_rx_control.valid &: (uart_rx_control.value ==:. 1))
                [ if_
                    (char_idx.value ==:. 0)
                    [ sum_idx <--. 0
                    ; part2_sum <--. 0
                    ; state_rd_addr <--. 0
                    ; sm.set_next Sum_read
                    ]
                  @@ else_
                       [ last_line <-- vdd
                       ; col_idx <--. 0
                       ; line_rd_addr <--. 0
                       ; state_rd_addr <--. 0
                       ; sm.set_next Process_read
                       ]
                ]
            ] )
        ; Process_read, [ (* Wait for RAM read *) sm.set_next Process_execute ]
        ; ( Process_execute
          , [ (* Python: for j in range(m): if line[j] == "^": ... elif line[j] == "S": ... *)
              if_
                (char_is_caret line_rd_data &: (state_rd_data >:. 0))
                [ (* Hit! res += 1, clear state[j], then spread to neighbors *)
                  result <-- result.value +:. 1
                ; saved_beam <-- state_rd_data
                ; state_wr_addr <-- col_idx.value
                ; state_wr_data <--. 0
                ; state_wr_en <-- vdd
                ; (* Set up spread: try left first, mark if right is pending *)
                  spread_right_pending <-- (col_idx.value <: line_len.value -:. 1)
                ; if_
                    (col_idx.value >:. 0)
                    [ (* Spread left: update_beam(line, j-1, beam) *)
                      spread_target <-- col_idx.value -:. 1
                    ; line_rd_addr <-- col_idx.value -:. 1
                    ; state_rd_addr <-- col_idx.value -:. 1
                    ; sm.set_next Spread_read
                    ]
                  @@ elif
                       (col_idx.value <: line_len.value -:. 1)
                       [ (* No left, spread right: update_beam(line, j+1, beam) *)
                         spread_target <-- col_idx.value +:. 1
                       ; line_rd_addr <-- col_idx.value +:. 1
                       ; state_rd_addr <-- col_idx.value +:. 1
                       ; spread_right_pending <--. 0
                       ; sm.set_next Spread_read
                       ]
                  @@ else_ [ sm.set_next Process_next ]
                ]
              @@ elif
                   (char_is_start line_rd_data)
                   [ (* Python: state[j] = 1 *)
                     state_wr_addr <-- col_idx.value
                   ; state_wr_data <--. 1
                   ; state_wr_en <-- vdd
                   ; sm.set_next Process_next
                   ]
              @@ else_ [ sm.set_next Process_next ]
            ] )
        ; ( Spread_read
          , [ (* Wait for RAM read of char[target] and state[target] *)
              sm.set_next Spread_write
            ] )
        ; ( Spread_write
          , [ (* update_beam - if line[target] == ".": state[target] += beam *)
              when_
                (char_is_dot line_rd_data)
                [ state_wr_addr <-- spread_target.value
                ; state_wr_data <-- state_rd_data +: saved_beam.value
                ; state_wr_en <-- vdd
                ]
            ; (* Check if we still need to spread right *)
              if_
                spread_right_pending.value
                [ spread_target <-- col_idx.value +:. 1
                ; line_rd_addr <-- col_idx.value +:. 1
                ; state_rd_addr <-- col_idx.value +:. 1
                ; spread_right_pending <--. 0
                ; sm.set_next Spread_read
                ]
              @@ else_ [ sm.set_next Process_next ]
            ] )
        ; ( Process_next
          , [ if_
                (col_idx.value ==: line_len.value -:. 1)
                [ col_idx <--. 0
                ; char_idx <--. 0
                ; if_
                    last_line.value
                    [ sum_idx <--. 0
                    ; part2_sum <--. 0
                    ; state_rd_addr <--. 0
                    ; sm.set_next Sum_read
                    ]
                  @@ else_ [ sm.set_next Read_line ]
                ]
              @@ else_
                   [ incr col_idx
                   ; line_rd_addr <-- col_idx.value +:. 1
                   ; state_rd_addr <-- col_idx.value +:. 1
                   ; sm.set_next Process_read
                   ]
            ] )
        ; Sum_read, [ sm.set_next Sum_accumulate ]
        ; ( Sum_accumulate
          , [ (* Python: sum(state) *)
              part2_sum <-- part2_sum.value +: state_rd_data
            ; if_ (sum_idx.value ==: line_len.value -:. 1) [ sm.set_next Done ]
              @@ else_
                   [ incr sum_idx
                   ; state_rd_addr <-- sum_idx.value +:. 1
                   ; sm.set_next Sum_read
                   ]
            ] )
        ; Done, []
        ]
    ];
  let done_ = sm.is Done in
  let%tydi { uart_tx } =
    Print_decimal_outputs.hierarchical
      scope
      { clock
      ; clear
      ; part1 = uresize ~width:60 result.value
      ; part2 = uresize ~width:60 part2_sum.value
      ; done_
      ; uart_tx_ready
      }
  in
  { board_leds = uresize ~width:8 done_; uart_tx; uart_rx_ready = uart_rx_ready.value }
;;

let hierarchical scope i =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~here:[%here] ~scope create i
;;
