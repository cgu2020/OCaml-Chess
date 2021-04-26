open OUnit2
open Chess_board
open Tile
open Command

let test_board = 
  "r,n,b,k,q,b,n,r/p,p,p,p,p,p,p,p/ , , , , , , , / , , , , , , , / , , , , , , , / , , , , , , , /P,P,P,P,P,P,P,P/R,N,B,K,Q,B,N,R"
  
let b = init let c = initialize b (string_to_lists test_board) 0


let board_test name row col expected_output : test =
  name >:: fun _ ->
  (* the [printer] tells OUnit how to convert the output to a string *)
  assert_equal expected_output ( get_piece b.(row).(col))

let possible_moves_tests = [ board_test "Test 1" 0 0 Rook]

let suite =
  "test suite for A2"
  >::: List.flatten [possible_moves_tests]

let _ = run_test_tt_main suite