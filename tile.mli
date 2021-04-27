type tile 
type p = Empty | Rook | Bishop | Knight | Pawn | Queen | King
type c = None | White | Black

val point_value : tile -> int

val print_piece : p -> unit

val get_piece : tile -> p

val get_color : tile -> c

val get_position : tile -> int*int

val empty_tile : tile

val pawn : tile

val empty_tile : tile

val parse_piece : string -> int -> int -> tile
