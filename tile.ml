type p = Empty | Rook | Bishop | Knight | Pawn | Queen | King
type c = None | White | Black

type tile = {
  piece : p;
  color : c;
  position : int*int;
}


let get_piece (t:tile) = t.piece

let print_piece t = match t with
|Pawn -> print_string "Pawn"
|Knight -> print_string "Knight"
|Rook -> print_string "Rook"
|Bishop -> print_string "Bishop"
|Queen -> print_string "Queen"
|King -> print_string "King"
|Empty -> print_string "Empty"

let get_color (t:tile) = t.color

let get_position (t:tile) = t.position

let point_value (t:tile) : int =
  match t.piece with
  |Empty -> 0
  |Rook -> 5
  |Bishop -> 3
  |Knight -> 3
  |Pawn -> 1
  |Queen -> 9
  |King -> 0

  let empty_tile = {
    piece = Empty;
    color = None;
    position = (0,0);
  }

  let pawn = {
  piece = Pawn;
  color = White;
  position = (0,0);
}
let check_capital (s:string) : bool = if String.equal (String.uppercase_ascii s) s then true else false

let parse_piece (s: string) (r:int) (c:int) : tile = 
  let piece = match String.lowercase_ascii s with 
    | "r" -> Rook
    | "n" -> Knight
    | "b" -> Bishop
    | "k" -> King
    | "q" -> Queen
    | "p" -> Pawn
    | _ -> Empty
  in let color = if piece == Empty then None else if check_capital s then White else Black in
  {
    piece = piece;
    color = color;
    position = (r, c);
  }