type tile = Empty | Rook | Bishop | Knight | Pawn | Queen | King

type team = int

let point_value t1 =
  match t1 with
  |Empty -> 0
  |Rook -> 5
  |Bishop -> 3
  |Knight -> 3
  |Pawn -> 1
  |Queen -> 9
  |King -> 0

