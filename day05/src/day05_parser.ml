open! Core
open! Advent_of_fpga_kernel
open! Advent_of_fpga_utils

(* Parse each range line and send start/end as separate U64 values *)
let parse input =
  let lines = String.split_lines input in
  let range_lines = List.filter lines ~f:(fun line -> String.contains line '-') in
  let data =
    range_lines
    |> List.concat_map ~f:(fun line ->
      match String.split line ~on:'-' with
      | [ start_str; end_str ] ->
        let start_value = Int.of_string (String.strip start_str) in
        let end_value = Int.of_string (String.strip end_str) in
        (* Send start and end as two separate U64 values *)
        Numeric_shifter.U64.For_parser.int_to_uart_symbols start_value
        @ Numeric_shifter.U64.For_parser.int_to_uart_symbols end_value
      | _ -> failwith ("Invalid range format: " ^ line))
  in
  data @ [ Control_byte '\x01' ]
;;
