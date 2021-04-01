type tile 
type p = Empty | Rook | Bishop | Knight | Pawn | Queen | King
type c = None | White | Black

val point_value : tile -> int

val get_piece : tile -> p

val get_color : tile -> c

val get_position : tile -> int*int

val empty_tile : tile

val pawn : tile
