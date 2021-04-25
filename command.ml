open Str

type object_phrase = string list

type command =
  | Move of object_phrase
  | Quit

exception Empty

exception Malformed

let rec parse_helper lst =
  match lst with
  |[] -> true
  |h:: t-> if String.length h = 2 && 
    String.get h 0 >= 'a' && String.get h 0 <= 'h' 
  && String.get h 1 >= '1' && String.get h 1 <= '8' 
  then parse_helper t 
  else raise Malformed

let parse str =
  let lst = String.split_on_char ' ' str in
  if String.equal str "quit" then Quit else
  let fixedlst = List.filter (fun a -> not (String.equal a "")) lst in
  if List.length fixedlst = 0 then raise Empty else
  if List.length fixedlst = 2 && parse_helper fixedlst then Move fixedlst
  else raise Malformed