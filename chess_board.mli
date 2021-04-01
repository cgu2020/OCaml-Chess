
(** The abstract type that represents a chessboard. *)
type board

exception InvalidMove

(** [possible_moves a] is a set-like list of all the tiles 
    that a can move to.*)
val possible_moves : board -> board list

val replace_tile : int -> int -> int -> int -> unit (**Helper for move piece*)

val clear_tile : board -> unit (**Helper for move_piece*)

(** [move_piece a b] clears tile a and replaces tile b with tile a
Raises [Invalid t] if [t] is not a valid tile to move to.  *)
val move_piece : board -> int -> int -> int-> int -> unit

val print_board : board -> unit