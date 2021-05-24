open Js_of_ocaml

type tile

type p =
  | Empty
  | Rook
  | Bishop
  | Knight
  | Pawn
  | Queen
  | King

type c =
  | None
  | White
  | Black

type img =
  | Image of string
  | No

(** [point_value t] represents the point value of t. *)
val point_value : tile -> int

(** [print_piece p] prints a string representation of the piece p. *)
val print_piece : p -> unit

(** [get_piece t] returns the piece on tile t. *)
val get_piece : tile -> p

(** [get_color t] returns the color on tile t. *)
val get_color : tile -> c

(** [get_img t] returns the image of tile t. *)
val get_img : tile -> img

(** [empty_tile] is the empty tile. *)
val empty_tile : tile

(** [pawn] is the pawn tile. *)
val pawn : tile

(** parse_piece s x y returns the tile with corresponding piece, color,
    img, of the string s.*)
val parse_piece : string -> int -> int -> tile
