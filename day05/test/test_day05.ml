open! Core

let%expect_test "test day05" =
  Advent_of_fpga_infra.Test_harness.run_combined_exn
    ~debug:false
    ~save_waves:false
    ~num_cycles:2000000
    ~input_filename:"day05.txt"
    (module Day05);
  [%expect {|
    === Output ===
    Part 1: 558
    Part 2: 344813017450467
    |}]
;;
