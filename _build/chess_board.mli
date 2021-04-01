(**

(** The abstract type that represents a chessboard. *)
type board

(** The abstract type that represents a chessboard. *)
type t

exception InvalidMove of t

(** [check_validity a b] is a boolean of whether moving tile a
    to tile b is a valid move*)
val check_validity : t -> t -> bool

(** [possible_moves a] is a set-like list of all the tiles 
    that a can move to.*)
val possible_moves : t -> t list

val replace_tile : int -> int -> int -> int -> unit (**Helper for move piece*)

val clear_tile : t -> unit (**Helper for move_piece*)

(** [move_piece a b] clears tile a and replaces tile b with tile a
Raises [Invalid t] if [t] is not a valid tile to move to.  *)
val move_piece : board -> int -> int -> int-> int -> unit
*)
