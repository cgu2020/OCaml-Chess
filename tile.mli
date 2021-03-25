type p
type c

type tile = {
  piece : p;
  color : c;
  coord : int * int
}

val point_value : tile -> int

val get_piece : tile -> p