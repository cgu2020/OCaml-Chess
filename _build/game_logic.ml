open Tile
(**val move_queen : tile -> tile list

val move_king : tile -> tile list

val move_pawn : tile -> tile list

val move_knight : tile -> tile list

val move_bishop : tile -> tile list

val move_king : tile -> tile list*)

exception InvalidMove
let check_validity (tile1 : Tile.tile) (tile2: Tile.tile) =
  let x = Tile.get_piece tile1 in 
  match x with 
  |Pawn -> true
  |_ -> false


(*  match tile1 with 
  | Empty -> raise NonValidFirstPiece
  | _  ->
    if tile2 = Empty then true (*Change empty to tile 2 in in the list of move_piecename *)
    else if tile2.team = tile1.team then 
      raise NonValidMove
    else false
*)

