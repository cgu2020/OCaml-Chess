open Tile
exception InvalidMove

type board = Tile.tile array array

type c = None | White | Black

(* The next 4 functions are helpers for checking the vertical and horizontal
possible moves.*)
let rec check_left_horz row col acc init_color board = 
  if col > 0 then 
    if get_piece board.(row).(col - 1) = Empty then 
      check_left_horz (row) (col - 1) ((row,col) :: acc) init_color board
    else if get_color board.(row).(col - 1) <> init_color then
      (row,col) :: (row, col - 1) :: acc
    else 
      (row,col) :: acc
  else (row,col) :: acc

let rec check_right_horz row col acc init_color board = 
  if col < 7 then 
    if get_piece board.(row).(col + 1) = Empty then 
      check_right_horz (row) (col + 1) ((row,col) :: acc) init_color board
    else if get_color board.(row).(col + 1) <> init_color then
      (row,col) :: (row, col + 1) :: acc
    else 
      (row,col) :: acc
  else (row,col) :: acc
  
let rec check_up_vert row col acc init_color board = 
  if row > 0 then 
    if get_piece board.(row - 1).(col) = Empty then 
      check_up_vert (row - 1) (col) ((row,col) :: acc) init_color board
    else if get_color board.(row - 1).(col) <> init_color then
      (row,col) :: (row - 1, col) :: acc
    else 
      (row,col) :: acc
  else (row,col) :: acc

let rec check_down_vert row col acc init_color board = 
  if row < 7 then 
    if get_piece board.(row + 1).(col) = Empty then 
      check_down_vert (row + 1) (col) ((row,col) :: acc) init_color board
    else if get_color board.(row + 1).(col) <> init_color then
      (row,col) :: (row + 1, col) :: acc
    else 
      (row,col) :: acc
  else (row,col) :: acc
   
(* The next 4 functions are helpers for checking the diagonal possible moves.*)
let rec diagonal_up_left row col acc init_color board = 
  if row > 0 && col > 0 then
    if get_piece board.(row - 1).(col - 1) = Empty then 
      diagonal_up_left (row - 1) (col - 1) ((row,col) :: acc) init_color board
    else if get_color board.(row - 1).(col - 1) <> init_color then
      (row,col) :: (row - 1, col - 1) :: acc
    else 
      (row,col) :: acc
  else (row,col) :: acc

let rec diagonal_up_right row col acc init_color board = 
  if row > 0 && col < 7 then 
    if get_piece board.(row - 1).(col + 1) = Empty then 
      diagonal_up_right (row - 1) (col + 1) ((row,col) :: acc) init_color board
    else if get_color board.(row - 1).(col + 1) <> init_color then
      (row,col) :: (row - 1, col + 1) :: acc
    else 
      (row,col) :: acc
  else (row,col) :: acc

let rec diagonal_down_right row col acc init_color board = 
  if row < 7 && col < 7 then 
    if get_piece board.(row + 1).(col + 1) = Empty then 
      diagonal_down_right (row + 1) (col + 1) ((row,col) :: acc) init_color board
    else if get_color board.(row + 1).(col + 1) <> init_color then
      (row,col) :: (row + 1, col + 1) :: acc
    else 
      (row,col) :: acc
  else (row,col) :: acc

let rec diagonal_down_left row col acc init_color board = 
    if row < 7 && col > 0 then 
      if get_piece board.(row + 1).(col - 1) = Empty then 
        diagonal_down_left (row + 1) (col - 1) ((row,col) :: acc) init_color board
      else if get_color board.(row + 1).(col - 1) <> init_color then
        (row,col) :: (row + 1, col - 1) :: acc
      else 
        (row,col) :: acc
    else (row,col) :: acc

(* Uses the helper methods in order to consolidate all of the diagonal 
positions given a certain piece *)
let diagonal row col init_color board =
  let lst = 
    diagonal_down_left row col [] init_color board @ 
    diagonal_up_right row col [] init_color board @ 
    diagonal_up_left row col [] init_color board @ 
    diagonal_down_right row col [] init_color board in 
    List.filter (fun (x,y) -> x <> row || y <> col) lst

(* Uses the helper methods in order to find all of the possible horizontal and 
vertical moves that a piece can do.*)
let horiz_vert_posib row col init_color board= 
  let lst = 
    check_left_horz row col [] init_color board @ 
    check_right_horz row col [] init_color board @ 
    check_up_vert row col [] init_color board @ 
    check_down_vert row col [] init_color board
    in
    List.filter (fun (x,y) -> x <> row || y <> col) lst

(* Checks all of the places that the king can move to *)
let valid_features x y init_color board = 
  x >= 0 && x <= 7 && y >= 0 && y <= 7 && not (get_color board.(x).(y) = init_color)

(* Updated so that if the piece is of the same color it cant move there. *)
let king_move row col init_color board = 
  let lst = [(row-1,col-1);(row-1,col);(row-1,col+1);(row, col-1);
  (row, col+1); (row+1,col-1);(row+1,col); (row+1,col+1)] in 
  List.filter (fun (x,y) -> valid_features x y init_color board) lst

let print_coor x y =
  print_int x; print_string ","; print_int y; print_string "THIS IS FROM KNIGHT_MOVES\n"
let knight_moves row col init_color board =  
  print_coor row col;
  let lst = [(row+2,col-1);(row+2,col+1);(row-2,col+1);(row-2,col-1);
  (row+1, col+2); (row-1,col+2);(row+1,col-2); (row-1,col-2)] in
  List.filter (fun (x,y) -> valid_features x y init_color board) lst

  let black_left_col row col board =
    if get_color board.(row + 1).(col + 1) = White then [(row+1,col+1)] else []
   
 let black_right_col row col board =
    if get_color board.(row + 1).(col - 1) = White then [(row+1,col-1)] else []
 let black_middle_col row col board =
  let lst = [] in
  if get_color board.(row + 1).(col + 1) = White
    then let lst2 = List.cons (row + 1,col + 1) lst in
      if get_color board.(row + 1).(col - 1) = White
        then List.cons (row+1,col-1) lst2
        else lst2
    else if get_color board.(row + 1).(col - 1) = White then [row+1,col-1] else []
  
 (*helper for pawn_attacked_tiles*)
 let black_pawn_attacks row col board =
  if col = 0 then black_left_col row col board
  else if col = 7 then black_right_col row col board else black_middle_col row col board
  
 let white_left_col row col board =
  if get_color board.(row - 1).(col + 1) = Black then [(row-1,col+1)] else []
  
 let white_right_col row col board =
  if get_color board.(row - 1).(col - 1) = Black then [(row-1,col-1)] else []
  
 let white_middle_col row col board =
  let lst = [] in
  if get_color board.(row - 1).(col + 1) = Black
    then let lst2 = List.cons (row - 1,col + 1) lst in
      if get_color board.(row - 1).(col - 1) = Black
        then List.cons (row-1,col-1) lst2
        else lst2
    else if get_color board.(row - 1).(col - 1) = Black then [row-1,col-1] else []
  
 let white_pawn_attacks row col board =
  if col = 0 then white_left_col row col board
  else if col = 7 then white_right_col row col board else white_middle_col row col board
let pawn_moves row col board = 
  let color = get_color board.(row).(col) in
  if color = White && row = 6 then List.append [(row-1, col); (row - 2, col)] (white_pawn_attacks row col board)
  else if color = Black && row = 1 then  List.append [(row+1, col); (row +2, col)] (black_pawn_attacks row col board)
  else if color = White then List.append [(row - 1, col)] (white_pawn_attacks row col board)
  else List.append [(row+1, col)] (black_pawn_attacks row col board)
 

(* Given a coordinate, it matches the piece type with the moves that the piece
is able to do, and returns the possible moves. *)
let possible_moves x y board = 
  let color = get_color board.(x).(y) in
  match get_piece board.(x).(y) with 
  | King ->  king_move x y color board
  | Queen -> horiz_vert_posib x y color board @ diagonal x y color board
  | Bishop -> diagonal x y color board
  | Rook -> horiz_vert_posib x y color board
  | Knight -> knight_moves x y color board
  | Pawn -> pawn_moves x y board
  | Empty -> []

let starterboard = 
  "r,n,b,k,q,b,n,r/p,p,p,p,p,p,p,p/ , , , , , , , / , , , , , , , / , , , , , , , / , , , , , , , /P,P,P,P,P,P,P,P/R,N,B,K,Q,B,N,R"
  
let string_to_lists (s:string) = let r = String.split_on_char '/' s in 
  List.map (fun x -> String.split_on_char ',' x) r

let rec initialize_row (b:board) (r:int) (c:int) (p: string list): unit = 
  match p with
  | [] -> ()
  | h::t -> b.(r).(c) <- parse_piece h r c; initialize_row b r (c+1) t 

let rec initialize (b:board) (s:string list list) (r:int): unit= 
  match s with 
  | [] -> ()
  | h::t -> initialize_row b r 0 h; initialize b t (r+1)

let init : board = Array.make_matrix 8 8 (empty_tile)

let rec print_pairs lst =
  match lst with
  |[] -> print_string ""
  |h :: t -> print_int (fst h); print_string ","; print_int (snd h); print_string "\n"; print_pairs t

let check_validity (b : board) (x : int) (y : int) (x2 : int) (y2 : int) (c :int ) : bool = 
  let snd_tile = (x2,y2) in
  let () = print_piece (get_piece b.(x).(y)); print_string "\n"; print_string "♜,♞,♝,♚,♛,♝,♞,♜/♟︎,♟︎,♟︎,♟︎,♟︎,♟︎,♟︎,♟︎/ , , , , , , , / , , , , , , , / , , , , , , , / , , , , , , , /♙,♙,♙,♙,♙,♙,♙,♙/♖,♘,♗,♔,♕,♗,♘,♖\n"; in
  print_string "Initial coord: "; print_int x; print_string ","; print_int y; print_string "\n";
  print_string "Possible tiles: "; print_pairs (possible_moves x y b);
  (not(get_piece b.(x).(y) = Empty)) &&
   ((get_color b.(x).(y) = Black && c mod 2 = 1)||(get_color b.(x).(y) = White && c mod 2 = 0)) && 
   List.mem snd_tile (possible_moves x y b)
  
(*We call check_validity in main so we assume this move_piece takes valid positions*)
let move_piece (b : board) (x : int) (y : int) (x2 : int) (y2 : int) (c :int ) : unit = 
  let piece1 = b.(x).(y) in
  b.(x2).(y2) <- piece1; b.(x).(y) <- empty_tile
