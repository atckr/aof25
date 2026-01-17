open! Core
open! Advent_of_fpga_kernel
open! Advent_of_fpga_utils

(* Encode characters as 2-bit values: 0='.' 1='S' 2='^' 3=other *)
let encode_char = function
  | '.' -> 0
  | 'S' -> 1
  | '^' -> 2
  | _ -> 3
;;

let parse s =
  let open Parser.Symbol in
  let lines = String.split_lines s in
  let line_width =
    match lines with
    | [] -> 0
    | hd :: _ -> String.length hd
  in
  let width_byte = Data_byte (Char.of_int_exn line_width) in
  let data =
    lines
    |> List.concat_map ~f:(fun line ->
         List.init line_width ~f:(fun i ->
           Data_byte (Char.of_int_exn (encode_char (String.get line i)))))
  in
  [ width_byte ] @ data @ [ Control_byte '\x01' ]
;;
