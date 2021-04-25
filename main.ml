open Chess_board
open Tile
open Command

let get_int c =
  match c with
|'1' -> 1
|'2' -> 2
|'3' -> 3
|'4' -> 4
|'5' -> 5
|'6' -> 6
|'7' -> 7
|'8' -> 8
|_ -> failwith "impossible"


let str_to_coord s = 
  let y = get_int (String.get s 1) in 
  match String.get s 0 with
  |'a' -> (1, y)
  |'b' -> (2, y)
  |'c' -> (3, y)
  |'d' -> (4, y)
  |'e' -> (5, y)
  |'f' -> (6, y)
  |'g' ->(7, y)
  |'h' -> (8, y)
  | _ -> failwith "impossible"

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

let rec convert_lst_to_coords lst acc = match lst with
|[] -> acc
|h :: t ->  convert_lst_to_coords t ((parse h) :: acc)

let rec play_game = let () = print_string "Enter two valid positions (e.g. a2 a4)" 
  in let i = read_line () in if i = "a1 a2" then print_string "true" else print_string "false"

let main () = 
  let b = init in
  initialize b (string_to_lists starterboard) 0;
  print b;
  play_game

let () = main()