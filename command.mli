type object_phrase = string list

type command =
  | Go of object_phrase
  | Quit

exception Empty

exception Malformed

val parse : string -> command