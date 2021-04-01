(** The abstract type that represents a chessboard. *)
type board = Tile.tile array array

exception InvalidMove

(** [possible_moves a] is a set-like list of all the tiles 
    that a can move to.*)
val possible_moves : Tile.tile -> int*int -> (int * int) list

(** [check_validity a b] is a boolean of whether moving tile a
    to tile b is a valid move*)
(**val check_validity : board -> int -> int -> int -> int -> bool*)


val replace_tile : int -> int -> int -> int -> unit (**Helper for move piece*)

val clear_tile : board -> int -> int -> unit (**Helper for move_piece*)

val string_to_lists : string -> string list list

val starterboard : string

val initialize : board -> string list list -> int -> unit

val init : board

(** [move_piece a b] clears tile a and replaces tile b with tile a
Raises [Invalid t] if [t] is not a valid tile to move to.  *)
val move_piece : board -> int -> int -> int-> int -> unit
