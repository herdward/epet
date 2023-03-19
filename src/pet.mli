

type pet
(** The abstract type of values representing pets *)

val from_json : Yojson.Basic.t -> pet

val getHealth : pet -> int

val getHunger : pet -> int

val getDescription : pet -> string

val getName : pet -> string
