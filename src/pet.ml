type pet = {
  name : string;
  gender : string;
  description : string;
  health : int;
  hunger : int; (*hygiene: int;*)
}

type pets = { pets : pet list }

open Yojson.Basic.Util

let pet_of_json pet_json =
  {
    name = pet_json |> member "name" |> to_string;
    gender = pet_json |> member "gender" |> to_string;
    description = pet_json |> member "description" |> to_string;
    health = pet_json |> member "health" |> to_int;
    hunger = pet_json |> member "hunger" |> to_int;
  }

let pets_of_json pets_json =
  { pets = pets_json |> member "pets" |> to_list |> List.map pet_of_json }

let get_pet pets name = List.find (fun pet -> pet.name = name) pets.pets
let get_health pet = pet.health
let get_gender pet = pet.gender
let get_hunger pet = pet.hunger
let get_description pet = pet.description
let get_name pet = pet.name

let update_pet_hunger pet 5 =
  let current_pet_hunger = get_hunger pet in
  {
    name = get_name pet;
    gender = get_gender pet;
    description = get_description pet;
    health = get_health pet;
    hunger = current_pet_hunger + 5;
  }
