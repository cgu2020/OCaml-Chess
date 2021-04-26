type object_phrase = string list

type command =
  | Move of object_phrase
  | InvalidMove
  | Quit

exception Empty

exception Malformed

val parse : string -> command