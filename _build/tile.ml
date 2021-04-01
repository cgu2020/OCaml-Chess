type p = Empty | Rook | Bishop | Knight | Pawn | Queen | King
type c = None | White | Black

type tile = {
  piece : p;
  color : c;
}

let get_piece (t:tile) = t.piece

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
  }

  let pawn = {
  piece = Pawn;
  color = White
}
