open OUnit2
open Chess_board
open Tile
open Command

let test_board =
  "r,n,b,k,q,b,n,r/p, ,p,p, , , , / ,p, , ,p,p, , / , , ,R, , ,p, / , \
   , , , , , , / , ,P,P, ,N, ,p/P,P, , ,P,P,P,P/ ,N,B,K,Q,B, ,R"

(*let board2 = "r, ,b,k,q,b,n,r/p,p,p, ,p,p,p,p / , ,n, , , , , / , ,
  ,p, , , , / , \ , , ,P, , , / , , , , , , , /P,P,P,P,
  ,P,P,P/R,N,B,K,Q,B,N,R"*)

(* 0 r n b k q b n r 1 p - p p - - - - 2 - p - - p p - - 3 - - - R - - p
   - 4 - - - - - - - - 5 - - P P - N - p 6 P P - - P P P P 7 - N B K Q B
   - R 0 1 2 3 4 5 6 7 *)

(* 0 r n b k q b n r 1 p p p p p p p p 2 - - - - - - - - 3 - - - p - - -
   - 4 - - - - P - - - 5 - - - - - - - - 6 P P P P - P P P 7 R N B K Q B
   N R 0 1 2 3 4 5 6 7 *)
let b = init

let c = initialize b (string_to_lists test_board) 0

(*let b2 = init let c = initialize b2 (string_to_lists board2) 0*)

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

let get_piece_test name row col expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (get_piece b.(row).(col))

let get_color_test name row col expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (get_color b.(row).(col))

let point_value_test name row col expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (point_value b.(row).(col))

let posib_moves_test name row col expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (possible_moves row col b)
    ~cmp:cmp_set_like_lists

let white_score_test name b x y x2 y2 expected_output : test =
  name >:: fun _ ->
  let filler = check_validity b x y x2 y2 0 in
  move_piece b x y x2 y2;
  assert_equal expected_output get_white_score

let black_score_test name expected_output : test =
  name >:: fun _ -> assert_equal expected_output get_black_score

let check_tiles_test name row col row2 col2 b expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    [ get_piece b.(row).(col); get_piece b.(row2).(col2) ]

(*let move_piece_test name x y x2 y2 expected_output : test = name >::
  fun _ -> let t = move_piece b 1 0 2 0 in assert_equal expected_output
  (move_piece b 1 0 2 0)*)

let tile_tests =
  [
    get_piece_test "Rook Check" 0 0 Rook;
    get_piece_test "Knight Check" 0 1 Knight;
    get_piece_test "Bishop Check" 0 2 Bishop;
    get_piece_test "Queen Check" 0 4 Queen;
    get_piece_test "King Check" 7 3 King;
    get_piece_test "Rook Check" 3 0 Empty;
    get_color_test "White Piece Color Check" 0 5 Black;
    get_color_test "Black Piece Color Check" 6 6 White;
    get_color_test "No Piece Color Check" 3 0 None;
    point_value_test "Rook Value" 0 0 5;
    point_value_test "Empty Value" 3 0 0;
  ]

let posib_moves_tests =
  [
    posib_moves_test "Black Rook Blocked in Corner" 0 0 [];
    posib_moves_test "White Rook in Middle of Board" 3 3
      [
        (3, 0);
        (3, 1);
        (3, 2);
        (3, 4);
        (3, 5);
        (3, 6);
        (4, 3);
        (2, 3);
        (1, 3);
      ];
    posib_moves_test "White Queen with Blocking Pawn" 7 4 [ (6, 3) ];
    posib_moves_test "Black Queen with Diagonal/Vert" 0 4
      [ (1, 4); (1, 5); (2, 6); (3, 7) ];
    posib_moves_test "White King Moves" 7 3 [ (6, 2); (6, 3) ];
    posib_moves_test "White Knight in the Middle\n      of Board" 5 5
      [ (4, 7); (3, 6); (4, 3); (3, 4); (7, 6); (6, 3) ];
    posib_moves_test "White Bishop Attacking Black Pawn" 7 2
      [ (6, 3); (5, 4); (4, 5); (3, 6) ];
    posib_moves_test "(1,3) Black Pawn\n      initial move with Blocker"
      1 3 [ (2, 3) ];
    posib_moves_test "(2,4)\n      Black Pawn with Capture" 2 4
      [ (3, 3); (3, 4) ];
    posib_moves_test "(6,6) White Pawn Initial State with Capture" 6 6
      [ (5, 6); (4, 6); (5, 7) ];
    posib_moves_test "(5,7) White Pawn Blocked off" 5 7 [ (6, 6) ];
    posib_moves_test "(2,1) Normal Black Pawn Move" 2 1 [ (3, 1) ];
    check_tiles_test "6 0 and 5 0 Tiles with pawn up" 6 0 5 0
      (let _ = move_piece b 6 0 5 0 in
       b)
      [ Empty; Pawn ];
    (*white_score_test "No captures" b2 6 0 5 0 0;*)
  ]

let suite =
  "test suite for A2"
  >::: List.flatten [ tile_tests; posib_moves_tests ]

let _ = run_test_tt_main suite
