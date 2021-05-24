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

let checkmatepos =
  "r,n,b,k, ,b,n,r/p,p,p,p,p, ,p,p/ , , , , , , , / , , , , ,p, ,q/ , \
   , , ,P, , , / , , ,P, , , , /P,P,P, , ,P,P,P/R,N,B,K,Q,B,N,R"

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

let piece_to_string p =
  match p with
  | Pawn -> "♙"
  | Rook -> "♖"
  | Knight -> "♘"
  | Bishop -> "♗"
  | King -> "♔"
  | Queen -> "♕"
  | Empty -> "-"

let get_piece_test name bd row col expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (get_piece bd.(row).(col))
    ~printer:piece_to_string

let get_color_test name b row col expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (get_color b.(row).(col))

let point_value_test name b row col expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (point_value b.(row).(col))

let posib_moves_test name b row col expected_output : test =
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

let check_test name expected_output : test =
  name >:: fun _ -> assert_equal expected_output (in_check Tile.White)

let checkmate_test name b expected_output : test =
  name >:: fun _ -> assert_equal expected_output (is_checkmate b)
