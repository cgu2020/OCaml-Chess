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

val point_value : tile -> int

val print_piece : p -> unit

val get_piece : tile -> p

val get_color : tile -> c

val get_img : tile -> img

val empty_tile : tile

val pawn : tile

val empty_tile : tile

val parse_piece : string -> int -> int -> tile
