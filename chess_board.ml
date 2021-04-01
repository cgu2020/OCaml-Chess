open Tile
exception InvalidMove

type board = Tile.tile array array

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

let move_piece (b : board) (x : int) (y : int) (x2 : int) (y2 : int) : unit = 
  let piece1 = b.(x).(y) in
  b.(x2).(y2) <- piece1; b.(x).(y) <- empty_tile

let swap_tile (b : board) = failwith ""

let replace_tile (x: int) (y:int) (x2: int) (y2: int) : unit = failwith ""
  
let clear_tile (b: board) (x:int) (y:int) : unit = failwith ""

(**let check_validity x y x2 y2 : bool = failwith ""*)
