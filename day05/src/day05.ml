(* Day 5: Range gap calculation *)

open! Core
open! Advent_of_fpga_kernel

let identifier = "day05"
let parser = `Both_parts (module Day05_parser : Parser.S)
let design = `Both_parts (module Day05_design.Part2 : Design.S)
