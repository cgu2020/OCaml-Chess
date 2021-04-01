open Tile
exception InvalidMove
(**let check_validity (tile1 : Tile.tile) (tile2: Tile.tile) =
  let x = Tile.get_piece tile1 in 
  match tile1 with 
  |  -> raise InvalidMove
  | _  ->
    if tile2 = Empty then true (*Change empty to tile 2 in in the list of move_piecename *)
    else if tile2.team = tile1.team then 
      raise NonValidMove
    else false*)


(*val move_queen : tile -> tile list

val move_king : tile -> tile list

val move_pawn : tile -> tile list

val move_knight : tile -> tile list

val move_bishop : tile -> tile list

val move_king : tile -> tile list*)

type board = Tile.tile array array



let init : board = 
  Array.make_matrix 8 8 (Tile.empty_tile) 
  (*in 
  a.(1).(4) <- Tile.pawn*)

(** *)
let rec inc_horz_vert start curr_loc acc multiplier numbers = 
  match numbers with 
  | [] -> acc
  | h::t -> 
    if h != curr_loc then
      inc_horz_vert start curr_loc (start + h * multiplier :: acc) multiplier t
    else 
      inc_horz_vert start curr_loc acc multiplier t

(* given a tile, returns all of the horizontal and vertical moves that it can go to. *)
(*let horiz_vert_posib t acc = 
  let pos = get_position t in  
  let start_vert = (pos / 8)  in 
  let start_horz = (pos mod 8) in 
  inc_horz_vert (start_vert* 8) start_horz [] 1 [0;1;2;3;4;5;6;7] @ 
  inc_horz_vert start_horz start_vert [] 8 [0;1;2;3;4;5;6;7]*)


let rec horz start (row_num:int) lower upper acc = 
  if start > lower then 
    horz start row_num (lower + 1) upper ((row_num,lower)::acc)
  else if start < upper then 
    horz start row_num lower (upper - 1) ((row_num,upper)::acc)
  else acc

let rec vert start (col_num:int) lower upper acc = 
  if start > lower then 
    vert start col_num (lower + 8) upper ((lower,col_num)::acc)
  else if start < upper then 
    vert start col_num lower (upper - 8) ((upper,col_num)::acc)
  else acc
  (*)
let horiz_vert_posib t acc = 
  let pos_tupl = (get_position t) in  
  let pos = fst pos_tupl 
  let row_num = (pos / 8)  in 
  let col_num = (pos mod 8) in 
  horz pos row_num (row_num * 8) (row_num * 8 + 7) [] @
  vert pos col_num col_num (col_num + 56) []*)

let possible_moves p = 
  match get_piece p with 
  | King ->  false
  | Queen -> true


let check_move_validity (b:board) x y x2 y2 = 
  (*if x2,y2 is within the possible moves depending on the piece*)
  let init_tile = b.(x).(y) in 
  let final_tile = b.(x2).(y2) in 
  if get_color init_tile = get_color final_tile then
    false
  else
    true


  (*let move_piece (b : board) (x : int) (y : int) (x2 : int) (y2 : int) (p : tile): unit =
  if(valid_move b x y x2 y2) then 
    failwith("unimplemented")
else
  raise InvalidMove*)
  
  
