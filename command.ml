open Str

type object_phrase = string list

type command =
  | Go of object_phrase
  | Quit

exception Empty

exception Malformed

let rec parse_helper lst =
  match lst with
  |[] -> true
  |h :: t -> if String.length h = 2 && Str.string_match 
    (Str.regexp "[a-h]+$") (String.sub h 0 1) 0 && 
  Str.string_match (Str.regexp "[1-8]+$") (String.sub h 1 1) 0 
  then parse_helper t 
  else raise Malformed

let parse str =
  let lst = String.split_on_char ' ' str in
  if String.equal str "quit" then Quit else
  let fixedlst = List.filter (fun a -> not (String.equal a "")) lst in
  if List.length fixedlst = 2 && parse_helper fixedlst then Go fixedlst 
  else raise Malformed