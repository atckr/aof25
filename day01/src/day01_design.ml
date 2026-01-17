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

module States = struct
  type t =
    | Wait_for_input
    | Calculate_crosses
    | Process_crosses
    | Update_count
    | Update_part1
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create
  scope
  ({ clock; clear; uart_rx_data; uart_rx_control; uart_rx_overflow; uart_tx_ready } :
    _ I.t)
  : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let%tydi { value = { valid = data_in_valid; value = data_in } } =
    Numeric_shifter.U32.hierarchical
      scope
      { clock; clear; byte_in = uart_rx_data; enable = vdd }
  in
  let data_in = Input_value.unpack data_in in
  let open Always in
  let sm = State_machine.create (module States) spec in
  let uart_rx_ready = Variable.wire ~default:gnd () in
  let position = Variable.reg spec ~width:16 ~clear_to:(of_unsigned_int ~width:16 50) in
  let part1_count = Variable.reg spec ~width:16 in
  let part2_count = Variable.reg spec ~width:16 in
  let current_dir_packed = Variable.reg spec ~width:1 in
  let current_count = Variable.reg spec ~width:16 in
  let needed = Variable.wire ~default:(zero 16) () in
  (* calculate how much step needed until next zero *)
  compile
    [ sm.switch
        [ ( Wait_for_input
          , [ uart_rx_ready <-- vdd
            ; when_
                data_in_valid
                [ current_dir_packed
                  <-- (data_in.left_or_right |> Left_or_right.Of_signal.pack)
                ; current_count <-- data_in.value
                ; sm.set_next Calculate_crosses
                ]
            ; when_
                (uart_rx_control.valid &: (uart_rx_control.value ==:. 1))
                [ sm.set_next Done ]
            ] )
        ; ( Calculate_crosses
          , [ needed
              <-- mux2
                    (position.value ==:. 0)
                    (of_int_trunc ~width:16 100)
                    (mux2
                       (current_dir_packed.value ==:. 1) (* Right *)
                       (of_int_trunc ~width:16 100 -: position.value)
                       position.value)
            ; when_
                (current_count.value >=: needed.value)
                [ incr part2_count
                ; current_count <-- current_count.value -: needed.value
                ; sm.set_next Process_crosses
                ]
            ; when_
                (current_count.value <: needed.value)
                [ (* Didn't cross, update current_count to position +/- current_count *)
                  current_count
                  <-- mux2
                        (current_dir_packed.value ==:. 1) (* Right *)
                        (position.value +: current_count.value)
                        (position.value +:. 100 -: current_count.value)
                ; sm.set_next Update_part1
                ]
            ] )
        ; ( Process_crosses
          , [ when_
                (current_count.value >=:. 100)
                [ incr part2_count (* Subtract 100 from current_count *)
                ; current_count <-- current_count.value -:. 100
                ; sm.set_next Process_crosses
                ]
            ; when_ (current_count.value <:. 100) [ sm.set_next Update_count ]
            ] )
        ; ( Update_count
          , [ current_count
              <-- mux2
                    (current_dir_packed.value ==:. 1)
                    current_count.value
                    (mux2
                       (current_count.value ==:. 0)
                       (of_int_trunc ~width:16 0)
                       (of_int_trunc ~width:16 100 -: current_count.value))
            ; sm.set_next Update_part1
            ] )
        ; ( Update_part1
          , [ position
              <-- mux2
                    (current_count.value >=:. 100)
                    (current_count.value -:. 100)
                    current_count.value
            ; when_
                (mux2
                   (current_count.value >=:. 100)
                   (current_count.value -:. 100)
                   current_count.value
                 ==:. 0)
                [ incr part1_count ]
            ; sm.set_next Wait_for_input
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
      ; part1 = part1_count.value |> uextend ~width:60
      ; part2 = part2_count.value |> uextend ~width:60
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
