type pet
(** The abstract type of values representing pets *)

let from_json : Yojson.Basic.t -> pet = raise (Failure "not done")

let getHealth : pet -> string = raise (Failure "not done")

let getHunger : pet -> string = raise (Failure "not done")

let getDescription : pet -> string = raise (Failure "not done")

let getName : pet -> string = raise (Failure "not done")