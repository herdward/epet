

type pet
type pets
(** The abstract type of values representing pets *)



val pet_of_json : Yojson.Basic.t -> pet
val pets_of_json: Yojson.Basic.t -> pets

val get_pet: pets  -> string -> pet

val getHealth : pet -> int

val getHunger : pet -> int

val getDescription : pet -> string

val getName : pet -> string
