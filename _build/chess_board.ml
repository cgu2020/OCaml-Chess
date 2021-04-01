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
  let a =  Array.make_matrix 8 8 (Tile.empty_tile) in 
  let b = a.(1).(4) <- Tile.pawn in a

let move_piece (b : board) (x : int) (y : int) (x2 : int) (y2 : int) : unit =
  failwith ""
