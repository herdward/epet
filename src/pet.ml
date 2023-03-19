
type pet = {
  name: string;
  gender: string;
  description: string;
  health: int;
  hunger: int;
  (*hygiene: int;*)
}

open Yojson.Basic.Util

let from_json pet_json = 
  {
  name = pet_json |> member "name"  |> to_string;
  gender = pet_json |> member "gender" |> to_string;
  description = pet_json |> member "description" |> to_string;
  health = pet_json |> member "health" |> to_int;
  hunger = pet_json |> member "hunger" |> to_int; 
  }
let getHealth pet = pet.health

let getHunger pet = pet.hunger

let getDescription  pet = pet.description

let getName pet = pet.name;