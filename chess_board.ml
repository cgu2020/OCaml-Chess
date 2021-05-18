open Tile

exception InvalidMove

type board = Tile.tile array array

type c =
  | None
  | White
  | Black

type scoreboard = {
  mutable white_score : int;
  mutable black_score : int;
  mutable white_captured : Tile.p list;
  mutable black_captured : Tile.p list;
}

type kings = {
  mutable white_king : int * int;
  mutable black_king : int * int;
}

type attacked_positions = {
  mutable white : (int * int) list;
  mutable black : (int * int) list;
}

let k = { white_king = (7, 3); black_king = (0, 3) }

let attacking_pos = { white = []; black = [] }

let s =
  {
    white_score = 0;
    black_score = 0;
    white_captured = [];
    black_captured = [];
  }

(* The next 4 functions are helpers for checking the vertical and
   horizontal possible moves.*)
let rec check_left_horz row col acc init_color board =
  if col > 0 then
    if get_piece board.(row).(col - 1) = Empty then
      check_left_horz row (col - 1) ((row, col) :: acc) init_color board
    else if get_color board.(row).(col - 1) <> init_color then
      (row, col) :: (row, col - 1) :: acc
    else (row, col) :: acc
  else (row, col) :: acc

let rec check_right_horz row col acc init_color board =
  if col < 7 then
    if get_piece board.(row).(col + 1) = Empty then
      check_right_horz row (col + 1) ((row, col) :: acc) init_color
        board
    else if get_color board.(row).(col + 1) <> init_color then
      (row, col) :: (row, col + 1) :: acc
    else (row, col) :: acc
  else (row, col) :: acc

let rec check_up_vert row col acc init_color board =
  if row > 0 then
    if get_piece board.(row - 1).(col) = Empty then
      check_up_vert (row - 1) col ((row, col) :: acc) init_color board
    else if get_color board.(row - 1).(col) <> init_color then
      (row, col) :: (row - 1, col) :: acc
    else (row, col) :: acc
  else (row, col) :: acc

let rec check_down_vert row col acc init_color board =
  if row < 7 then
    if get_piece board.(row + 1).(col) = Empty then
      check_down_vert (row + 1) col ((row, col) :: acc) init_color board
    else if get_color board.(row + 1).(col) <> init_color then
      (row, col) :: (row + 1, col) :: acc
    else (row, col) :: acc
  else (row, col) :: acc

(* The next 4 functions are helpers for checking the diagonal possible
   moves.*)
let rec diagonal_up_left row col acc init_color board =
  if row > 0 && col > 0 then
    if get_piece board.(row - 1).(col - 1) = Empty then
      diagonal_up_left (row - 1) (col - 1) ((row, col) :: acc)
        init_color board
    else if get_color board.(row - 1).(col - 1) <> init_color then
      (row, col) :: (row - 1, col - 1) :: acc
    else (row, col) :: acc
  else (row, col) :: acc

let rec diagonal_up_right row col acc init_color board =
  if row > 0 && col < 7 then
    if get_piece board.(row - 1).(col + 1) = Empty then
      diagonal_up_right (row - 1) (col + 1) ((row, col) :: acc)
        init_color board
    else if get_color board.(row - 1).(col + 1) <> init_color then
      (row, col) :: (row - 1, col + 1) :: acc
    else (row, col) :: acc
  else (row, col) :: acc

let rec diagonal_down_right row col acc init_color board =
  if row < 7 && col < 7 then
    if get_piece board.(row + 1).(col + 1) = Empty then
      diagonal_down_right (row + 1) (col + 1) ((row, col) :: acc)
        init_color board
    else if get_color board.(row + 1).(col + 1) <> init_color then
      (row, col) :: (row + 1, col + 1) :: acc
    else (row, col) :: acc
  else (row, col) :: acc

let rec diagonal_down_left row col acc init_color board =
  if row < 7 && col > 0 then
    if get_piece board.(row + 1).(col - 1) = Empty then
      diagonal_down_left (row + 1) (col - 1) ((row, col) :: acc)
        init_color board
    else if get_color board.(row + 1).(col - 1) <> init_color then
      (row, col) :: (row + 1, col - 1) :: acc
    else (row, col) :: acc
  else (row, col) :: acc

(* Uses the helper methods in order to consolidate all of the diagonal
   positions given a certain piece *)
let diagonal row col init_color board =
  let lst =
    diagonal_down_left row col [] init_color board
    @ diagonal_up_right row col [] init_color board
    @ diagonal_up_left row col [] init_color board
    @ diagonal_down_right row col [] init_color board
  in
  List.filter (fun (x, y) -> x <> row || y <> col) lst

(* Uses the helper methods in order to find all of the possible
   horizontal and vertical moves that a piece can do.*)
let horiz_vert_posib row col init_color board =
  let lst =
    check_left_horz row col [] init_color board
    @ check_right_horz row col [] init_color board
    @ check_up_vert row col [] init_color board
    @ check_down_vert row col [] init_color board
  in
  List.filter (fun (x, y) -> x <> row || y <> col) lst

(* Checks all of the places that the king can move to *)
let valid_features x y init_color board =
  x >= 0 && x <= 7 && y >= 0 && y <= 7
  && not (get_color board.(x).(y) = init_color)

(* Updated so that if the piece is of the same color it cant move there. *)
let king_move row col init_color board =
  let lst =
    [
      (row - 1, col - 1);
      (row - 1, col);
      (row - 1, col + 1);
      (row, col - 1);
      (row, col + 1);
      (row + 1, col - 1);
      (row + 1, col);
      (row + 1, col + 1);
    ]
  in
  List.filter (fun (x, y) -> valid_features x y init_color board) lst

let print_coor x y =
  print_int x;
  print_string ",";
  print_int y

let knight_moves row col init_color board =
  (*print_coor row col;*)
  let lst =
    [
      (row + 2, col - 1);
      (row + 2, col + 1);
      (row - 2, col + 1);
      (row - 2, col - 1);
      (row + 1, col + 2);
      (row - 1, col + 2);
      (row + 1, col - 2);
      (row - 1, col - 2);
    ]
  in
  List.filter (fun (x, y) -> valid_features x y init_color board) lst

let black_left_col row col board =
  if get_color board.(row + 1).(col + 1) = White then
    [ (row + 1, col + 1) ]
  else []

let black_right_col row col board =
  if get_color board.(row + 1).(col - 1) = White then
    [ (row + 1, col - 1) ]
  else []

let black_middle_col row col board =
  let lst = [] in
  if get_color board.(row + 1).(col + 1) = White then
    let lst2 = List.cons (row + 1, col + 1) lst in
    if get_color board.(row + 1).(col - 1) = White then
      List.cons (row + 1, col - 1) lst2
    else lst2
  else if get_color board.(row + 1).(col - 1) = White then
    [ (row + 1, col - 1) ]
  else []

(*helper for pawn_attacked_tiles*)
let black_pawn_attacks row col board =
  if col = 0 then black_left_col row col board
  else if col = 7 then black_right_col row col board
  else black_middle_col row col board

let white_left_col row col board =
  if get_color board.(row - 1).(col + 1) = Black then
    [ (row - 1, col + 1) ]
  else []

let white_right_col row col board =
  if get_color board.(row - 1).(col - 1) = Black then
    [ (row - 1, col - 1) ]
  else []

let white_middle_col row col board =
  let lst = [] in
  if get_color board.(row - 1).(col + 1) = Black then
    let lst2 = List.cons (row - 1, col + 1) lst in
    if get_color board.(row - 1).(col - 1) = Black then
      List.cons (row - 1, col - 1) lst2
    else lst2
  else if get_color board.(row - 1).(col - 1) = Black then
    [ (row - 1, col - 1) ]
  else []

let white_pawn_attacks row col board =
  if col = 0 then white_left_col row col board
  else if col = 7 then white_right_col row col board
  else white_middle_col row col board

(*Removes the positions where a pawn is blocked by another piece*)
let rec remove_pawn_blocked row col board lst =
  match lst with
  | [] -> []
  | h :: t ->
      if
        h = (row - 2, col)
        && not (get_color board.(row - 2).(col) = None)
        || h = (row - 1, col)
           && not (get_color board.(row - 1).(col) = None)
        || h = (row + 1, col)
           && not (get_color board.(row + 1).(col) = None)
        || h = (row + 2, col)
           && not (get_color board.(row + 2).(col) = None)
      then remove_pawn_blocked row col board t
      else h :: remove_pawn_blocked row col board t

let pawn_moves row col board =
  let color = get_color board.(row).(col) in
  if color = White && row = 6 then
    List.append
      [ (row - 1, col); (row - 2, col) ]
      (white_pawn_attacks row col board)
    |> remove_pawn_blocked row col board
  else if color = Black && row = 1 then
    List.append
      [ (row + 1, col); (row + 2, col) ]
      (black_pawn_attacks row col board)
    |> remove_pawn_blocked row col board
  else if color = White then
    List.append [ (row - 1, col) ] (white_pawn_attacks row col board)
    |> remove_pawn_blocked row col board
  else
    List.append [ (row + 1, col) ] (black_pawn_attacks row col board)
    |> remove_pawn_blocked row col board

let white_left_col row col board = [ (row - 1, col + 1) ]

let white_right_col row col board = [ (row - 1, col - 1) ]

let white_middle_col row col board =
  [ (row - 1, col + 1); (row - 1, col - 1) ]

let white_pawn_attacking_squares row col board =
  if col = 0 then white_left_col row col board
  else if col = 7 then white_right_col row col board
  else white_middle_col row col board

let black_left_col row col board = [ (row + 1, col + 1) ]

let black_right_col row col board = [ (row + 1, col - 1) ]

let black_middle_col row col board =
  [ (row + 1, col + 1); (row + 1, col - 1) ]

let black_pawn_attacking_squares row col board =
  if col = 0 then black_left_col row col board
  else if col = 7 then black_right_col row col board
  else black_middle_col row col board

let pawn_attacking_moves row col b =
  let color = get_color b.(row).(col) in
  if color = Tile.White then white_pawn_attacking_squares row col b
  else black_pawn_attacking_squares row col b

(* Given a coordinate, it matches the piece type with the moves that the
   piece is able to do, and returns the possible moves. *)
let possible_moves x y board =
  let color = get_color board.(x).(y) in
  match get_piece board.(x).(y) with
  | King -> king_move x y color board
  | Queen -> horiz_vert_posib x y color board @ diagonal x y color board
  | Bishop -> diagonal x y color board
  | Rook -> horiz_vert_posib x y color board
  | Knight -> knight_moves x y color board
  | Pawn -> pawn_moves x y board
  | Empty -> []

let attacking_moves x y board =
  let color = get_color board.(x).(y) in
  match get_piece board.(x).(y) with
  | King -> king_move x y color board
  | Queen -> horiz_vert_posib x y color board @ diagonal x y color board
  | Bishop -> diagonal x y color board
  | Rook -> horiz_vert_posib x y color board
  | Knight -> knight_moves x y color board
  | Pawn -> pawn_attacking_moves x y board
  | Empty -> []

let starterboard =
  "r,n,b,k,q,b,n,r/p,p,p,p,p,p,p,p/ , , , , , , , / , , , , , , , / , \
   , , , , , , / , , , , , , , /P,P,P,P,P,P,P,P/R,N,B,K,Q,B,N,R"

let string_to_lists (s : string) =
  let r = String.split_on_char '/' s in
  List.map (fun x -> String.split_on_char ',' x) r

let rec initialize_row (b : board) (r : int) (c : int) (p : string list)
    : unit =
  match p with
  | [] -> ()
  | h :: t ->
      b.(r).(c) <- parse_piece h r c;
      initialize_row b r (c + 1) t

let rec initialize (b : board) (s : string list list) (r : int) : unit =
  match s with
  | [] -> ()
  | h :: t ->
      initialize_row b r 0 h;
      initialize b t (r + 1)

let init : board = Array.make_matrix 8 8 empty_tile

let rec print_pairs lst =
  match lst with
  | [] -> print_string ""
  | h :: t ->
      print_int (fst h);
      print_string ",";
      print_int (snd h);
      print_string "\n";
      print_pairs t

let rec update_attackers_row r c b wlst blst =
  match c with
  | 8 -> (wlst, blst)
  | y ->
      if get_color b.(r).(c) = White then
        update_attackers_row r (c + 1) b
          (wlst @ attacking_moves r c b)
          blst
      else if get_color b.(r).(c) = Black then
        update_attackers_row r (c + 1) b wlst
          (blst @ attacking_moves r c b)
      else update_attackers_row r (c + 1) b wlst blst

let rec update_attackers r b wlst blst =
  match r with
  | 8 -> (wlst, blst)
  | x ->
      let tup = update_attackers_row x 0 b [] [] in
      update_attackers (r + 1) b (wlst @ fst tup) (blst @ snd tup)

(*Precondition: c is the color of the king*)
let king_attacked x2 y2 c =
  match c with
  | Tile.White -> List.mem (x2, y2) attacking_pos.black
  | Tile.Black -> List.mem (x2, y2) attacking_pos.white
  | _ -> failwith "impossible"

let moving_in_check b x y x2 y2 c : bool =
  if get_piece b.(x).(y) = King then king_attacked x2 y2 c else false

let in_check c =
  match c with
  | Tile.White -> List.mem k.white_king attacking_pos.black
  | Tile.Black -> List.mem k.black_king attacking_pos.white
  | _ -> failwith "impossible"

let counter_check x y x2 y2 b c =
  match get_piece b.(x).(y) with
  | King -> (
      match c with
      | Tile.White -> not (List.mem (x2, y2) attacking_pos.black)
      | Tile.Black -> not (List.mem (x2, y2) attacking_pos.white)
      | _ -> failwith "impossible")
  | _ -> false

(* "need to implement this part if the moving piece isnt the king"*)

let check_validity
    (b : board)
    (x : int)
    (y : int)
    (x2 : int)
    (y2 : int)
    (c : int) : bool =
  let snd_tile = (x2, y2) in
  let bo =
    (*let () = print_piece (get_piece b.(x).(y)); print_string "\n";
      print_string "♜,♞,♝,♚,♛,♝,♞,♜/♟︎,♟︎,♟︎,♟︎,♟︎,♟︎,♟︎,♟︎/ , , , , , , , / , ,
      , , , , , / , , , , , , , / , , , , , , ,
      /♙,♙,♙,♙,♙,♙,♙,♙/♖,♘,♗,♔,♕,♗,♘,♖\n"; in print_string "Initial
      coord: "; print_int x; print_string ","; print_int y; print_string
      "\n"; print_string "Possible tiles: "; print_pairs (possible_moves
      x y b);*)
    (not (get_piece b.(x).(y) = Empty))
    && ((get_color b.(x).(y) = Black && c mod 2 = 1)
       || (get_color b.(x).(y) = White && c mod 2 = 0))
    && List.mem snd_tile (possible_moves x y b)
    && (if in_check (get_color b.(x).(y)) then
        counter_check x y x2 y2 b (get_color b.(x).(y))
       else true)
    && not (moving_in_check b x y x2 y2 (get_color b.(x).(y)))
    (*if get_color b.(x).(y) = White then not_moving_in_check b x y x2
      y2 White else not_moving_in_check b x y x2 y2 Black*)
  in
  if bo then
    if get_color b.(x2).(y2) = White then (
      s.black_score <- s.black_score + point_value b.(x2).(y2);
      s.white_captured <- s.white_captured @ [ get_piece b.(x2).(y2) ];
      true)
    else if get_color b.(x2).(y2) = Black then (
      s.white_score <- s.white_score + point_value b.(x2).(y2);
      s.black_captured <- s.black_captured @ [ get_piece b.(x2).(y2) ];
      true)
    else true
  else false

let get_white_score = s.white_score

let get_black_score = s.black_score

let get_captured_black = s.black_captured

let get_captured_white = s.white_captured

let rec is_checkmate klst alst =
  match klst with
  | h :: t -> List.mem h alst && is_checkmate t alst
  | [] -> true

let c_mates c b =
  match c with
  | Tile.Black ->
      let kings_moves =
        possible_moves (fst k.white_king) (snd k.white_king) b
      in
      in_check c && is_checkmate kings_moves attacking_pos.black
  | Tile.White ->
      let kings_moves =
        possible_moves (fst k.black_king) (snd k.black_king) b
      in
      in_check c && is_checkmate kings_moves attacking_pos.white
  | _ -> failwith "impossible"

(*We call check_validity in main so we assume this move_piece takes
  valid positions*)
let move_piece (b : board) (x : int) (y : int) (x2 : int) (y2 : int) :
    unit =
  if get_piece b.(x).(y) = King then
    if get_color b.(x).(y) = White then k.white_king <- (x2, y2)
    else k.black_king <- (x2, y2)
  else ();
  let piece1 = b.(x).(y) in
  b.(x2).(y2) <- piece1;
  b.(x).(y) <- empty_tile;
  let attacks = update_attackers 0 b [] [] in
  attacking_pos.white <- fst attacks;
  attacking_pos.black <- snd attacks
