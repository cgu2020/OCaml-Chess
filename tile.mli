open Js_of_ocaml

type tile 
type p = Empty | Rook | Bishop | Knight | Pawn | Queen | King
type c = None | White | Black
type img = Image of Dom_html.imageElement Js.t | No

val point_value : tile -> int

val get_piece : tile -> p

val get_color : tile -> c

val get_position : tile -> int*int

val get_img : tile -> img

val empty_tile : tile

val pawn : tile

val empty_tile : tile

val parse_piece : string -> int -> int -> tile
