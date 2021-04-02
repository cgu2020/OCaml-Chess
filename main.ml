open Chess_board
open Tile

let string_piece tile =
  match get_piece tile with
  |Pawn -> "p"
  |Rook -> "r"
  |Knight -> "n"
  |Bishop -> "b"
  |King -> "k"
  |Queen -> "q"
  |Empty -> ","

let rec print_row x y b: unit = match y with 
| 8 -> print_string "\n"
| y -> print_string (string_piece b.(x).(y) ^ "  "); print_row x (y+1) b

let rec print_board x b: unit =
  match x with
  | 8 -> ()
  | x -> print_row x 0 b; print_board (x+1) b

let print b= print_board 0 b

let main () = 
  let b = init in
  initialize b (string_to_lists starterboard) 0;
  print b

let () = main()