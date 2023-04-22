exception AlreadyFull

type food = {
  name : string;
  effect : int;
}

type pet = {
  name : string;
  gender : string;
  description : string;
  health : int;
  hunger : int; (*hygiene: int;*)
  bad_foods : food list;
}

let bad_food_of_json (j : Yojson.Basic.t) =
  {
    name =
      j
      |> Yojson.Basic.Util.member "bad food name"
      |> Yojson.Basic.Util.to_string;
    effect =
      j
      |> Yojson.Basic.Util.member "bad health effect"
      |> Yojson.Basic.Util.to_int;
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
    bad_foods =
      pet_json |> member "bad food" |> to_list |> List.map bad_food_of_json;
  }

let pets_of_json pets_json =
  { pets = pets_json |> member "pets" |> to_list |> List.map pet_of_json }

let get_pet pets name = List.find (fun pet -> pet.name = name) pets.pets
let get_health pet = pet.health
let get_gender pet = pet.gender
let get_hunger pet = pet.hunger
let get_description pet = pet.description
let get_name pet = pet.name
let get_bad_foods pet = pet.bad_foods

let get_bad_food (pet : pet) name : food =
  List.find (fun (food : food) -> food.name = name) pet.bad_foods

let get_bad_food_effect food = food.effect
let get_bad_food_name (food : food) : string = food.name

let update_pet_hunger pet food_value =
  let current_pet_hunger = get_hunger pet in
  if current_pet_hunger = 0 then raise AlreadyFull
  else
    {
      name = get_name pet;
      gender = get_gender pet;
      description = get_description pet;
      health = get_health pet;
      hunger = current_pet_hunger - food_value;
      bad_foods = pet.bad_foods;
    }

let update_pet_health pet food_health_effect =
  let current_pet_hunger = get_hunger pet in
  if current_pet_hunger = 0 then raise AlreadyFull
  else
    {
      name = get_name pet;
      gender = get_gender pet;
      description = get_description pet;
      health = get_health pet + food_health_effect;
      hunger = get_hunger pet;
      bad_foods = pet.bad_foods;
    }
