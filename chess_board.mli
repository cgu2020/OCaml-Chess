(** The abstract type that represents a chessboard. *)
type board = Tile.tile array array

type scoreboard

exception InvalidMove

(** [possible_moves a] is a set-like list of all the tiles that a can
    move to.*)
val possible_moves : int -> int -> board -> (int * int) list

(** [check_validity a b] is a boolean of whether moving tile a to tile b
    is a valid move*)

(**val check_validity : board -> int -> int -> int -> int -> bool*)

val string_to_lists : string -> string list list

val starterboard : string

val initialize : board -> string list list -> int -> unit

val init : board

val check_validity : board -> int -> int -> int -> int -> int -> bool

(** [move_piece b x1 y1 x2 y2 c] clears tile (x1,y1) and replaces tile
    (x2,y2) with tile (x1, y1). Invalid move if the int c does not match
    the color Raises [Invalid t] if [t] is not a valid tile to move to. *)
val move_piece : board -> int -> int -> int -> int -> unit
