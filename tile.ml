open Js_of_ocaml

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
  | Image of Dom_html.imageElement Js.t
  | No

type tile = {
  piece : p;
  color : c;
  mutable image : img;
}

let get_piece (t : tile) = t.piece

let print_piece t =
  match t with
  | Pawn -> print_string "Pawn"
  | Knight -> print_string "Knight"
  | Rook -> print_string "Rook"
  | Bishop -> print_string "Bishop"
  | Queen -> print_string "Queen"
  | King -> print_string "King"
  | Empty -> print_string "Empty"

let get_color (t : tile) = t.color

let get_img t = t.image

let point_value (t : tile) : int =
  match t.piece with
  | Empty -> 0
  | Rook -> 5
  | Bishop -> 3
  | Knight -> 3
  | Pawn -> 1
  | Queen -> 9
  | King -> 0

let img src =
  let i = Dom_html.createImg Dom_html.document in
  i##.src := Js.string src;
  i

let empty_tile = { piece = Empty; color = None; image = No }

let pawn =
  {
    piece = Pawn;
    color = White;
    image = Image (img "./images/WhitePawn.png");
  }

let check_capital (s : string) : bool =
  String.equal (String.uppercase_ascii s) s

let parse_piece (s : string) (r : int) (c : int) : tile =
  let piece =
    match String.lowercase_ascii s with
    | "r" -> Rook
    | "n" -> Knight
    | "b" -> Bishop
    | "k" -> King
    | "q" -> Queen
    | "p" -> Pawn
    | _ -> Empty
  in
  let color =
    if piece = Empty then None
    else if check_capital s then White
    else Black
  in
  let i =
    match (piece, color) with
    | Rook, White -> Image (img "./images/WhiteRook.png")
    | Bishop, White -> Image (img "./images/WhiteBishop.png")
    | Knight, White -> Image (img "./images/WhiteKnight.png")
    | King, White -> Image (img "./images/WhiteKing.png")
    | Queen, White -> Image (img "./images/WhiteQueen.png")
    | Pawn, White -> Image (img "./images/WhitePawn.png")
    | Rook, Black -> Image (img "./images/BlackRook.png")
    | Bishop, Black -> Image (img "./images/BlackBishop.png")
    | Knight, Black -> Image (img "./images/BlackKnight.png")
    | King, Black -> Image (img "./images/BlackKing.png")
    | Queen, Black -> Image (img "./images/BlackQueen.png")
    | Pawn, Black -> Image (img "./images/BlackPawn.png")
    | _, _ -> No
  in
  { piece; color; image = i }
