type tile 
type p = Empty | Rook | Bishop | Knight | Pawn | Queen | King
type c = None | White | Black

val point_value : tile -> int

val get_piece : tile -> p

val empty_tile : tile

val parse_piece : string -> int -> int -> tile