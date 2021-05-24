open Test
open OUnit2
open Chess_board
open Tile
open Command

let b = init

let _ = initialize b (string_to_lists starterboard) 0

let _ = move_piece b 6 5 5 5

let _ = move_piece b 1 4 3 4

let _ = move_piece b 6 6 4 6

let _ = move_piece b 0 3 4 7

let checkmate_tests = [ checkmate_test "Checkmate" b true ]

let suite = "test suite for Chess" >::: List.flatten [ checkmate_tests ]

let _ = run_test_tt_main suite
