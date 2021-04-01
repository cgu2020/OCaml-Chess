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

type board = tile array array

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

let possible_moves (b: board) : board list = failwith ""

let replace_tile (x: int) (y:int) (x2: int) (y2: int) : unit = failwith ""
  
let clear_tile (b: board) : unit = failwith ""

let print_board (b: board) : unit = failwith ""

let check_validity x y x2 y2 : bool = failwith ""