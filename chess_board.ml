open Tile
exception InvalidMove

type board = Tile.tile array array

(** *)
let rec inc_horz_vert start curr_loc acc multiplier numbers = 
  match numbers with 
  | [] -> acc
  | h::t -> 
    if h != curr_loc then
      inc_horz_vert start curr_loc (start + h * multiplier :: acc) multiplier t
    else 
      inc_horz_vert start curr_loc acc multiplier t

let rec horz start (row_num:int) lower upper acc = 
  if start > lower then 
    horz start row_num (lower + 1) upper ((row_num,lower mod 8)::acc)
  else if start < upper then 
    horz start row_num lower (upper - 1) ((row_num,upper mod 8)::acc)
  else acc

let rec vert start (col_num:int) lower upper acc = 
  if start > lower then 
    vert start col_num (lower + 8) upper ((lower / 8,col_num)::acc)
  else if start < upper then 
    vert start col_num lower (upper - 8) ((upper / 8,col_num)::acc)
  else acc

let rec diagonal_up_left row col acc fst = 
  if row > 0 && col > 0 && fst then 
    diagonal_up_left (row - 1) (col - 1) acc false 
  else if row > 0 && col > 0 then 
    diagonal_up_left (row - 1) (col - 1) ((row,col)::acc) false
  else if fst then acc
  else (row,col) :: acc

let rec diagonal_up_right row col acc fst = 
    if row > 0 && col < 8 && fst then 
      diagonal_up_right (row - 1) (col + 1) acc false 
    else if row > 0 && col < 8 then 
      diagonal_up_right (row - 1) (col + 1) ((row,col)::acc) false
    else if fst then acc
    else (row,col) :: acc

let rec diagonal_down_right row col acc fst = 
  if row < 8 && col < 8 && fst then 
    diagonal_down_right (row + 1) (col + 1) acc false 
  else if row < 8 && col < 8 then 
    diagonal_down_right (row + 1) (col + 1) ((row,col)::acc) false
  else if fst then acc
  else (row,col) :: acc

let rec diagonal_down_left row col acc fst = 
    if row < 8 && col > 0 && fst then 
      diagonal_down_left (row + 1) (col - 1) acc false 
    else if row < 8 && col > 0 then 
      diagonal_down_left (row + 1) (col - 1) ((row,col)::acc) false
    else if fst then acc
    else (row,col) :: acc

(** Important Big Boi *)
let diagonal cord =
  let row = fst cord in 
  let col = snd cord in
  diagonal_down_left row col [] true @ 
  diagonal_up_right row col [] true @ 
  diagonal_up_left row col [] true @ 
  diagonal_down_right row col [] true

let horiz_vert_posib cord = 
  let row = fst cord in 
  let col = snd cord in
  let pos = row*8+col in 
  horz pos row (row * 8) (row * 8 + 7) [] @
  vert pos col col (col + 56) []

let king_move cord = 
  let row = fst cord in 
  let col = snd cord in
  let lst = [(row-1,col-1);(row-1,col);(row-1,col+1);(row, col-1);
  (row, col+1); (row+1,col-1);(row+1,col); (row+1,col+1)] in 
  List.filter (fun (x,y) -> x >= 0 && x <= 7 && y >= 0 && y <= 7) lst

let possible_moves p cord = 
  match get_piece p with 
  | King ->  king_move cord
  | Queen -> horiz_vert_posib cord @ diagonal cord
  | Bishop -> diagonal cord
  | Rook -> horiz_vert_posib cord
  | _ -> []

let check_move_validity (b:board) x y x2 y2 = failwith "unimplemented"

