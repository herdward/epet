

type pet
type pets
(** The abstract type of values representing pets *)



val pet_of_json : Yojson.Basic.t -> pet
val pets_of_json: Yojson.Basic.t -> pets

val getHealth : pet -> int

val getHunger : pet -> int

val getDescription : pet -> string

val getName : pet -> string
