exception AlreadyFull
exception AlreadyHealthy
exception AlreadyClean

type food = {
  fname : string;
  health_effect : int;
  hunger_effect : int;
}

type pet = {
  name : string;
  gender : string;
  description : string;
  health : int;
  hunger : int;
  hygiene : int;
  bad_foods : food list;
  good_foods : food list;
}

let bad_food_of_json (j : Yojson.Basic.t) =
  {
    fname =
      j
      |> Yojson.Basic.Util.member "bad food name"
      |> Yojson.Basic.Util.to_string;
    health_effect =
      j
      |> Yojson.Basic.Util.member "bad health effect"
      |> Yojson.Basic.Util.to_int;
    hunger_effect =
      j |> Yojson.Basic.Util.member "hunger effect" |> Yojson.Basic.Util.to_int;
  }

let good_food_of_json (j : Yojson.Basic.t) =
  {
    fname =
      j
      |> Yojson.Basic.Util.member "good food name"
      |> Yojson.Basic.Util.to_string;
    health_effect =
      j
      |> Yojson.Basic.Util.member "good health effect"
      |> Yojson.Basic.Util.to_int;
    hunger_effect =
      j |> Yojson.Basic.Util.member "hunger effect" |> Yojson.Basic.Util.to_int;
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
    good_foods =
      pet_json |> member "good food" |> to_list |> List.map good_food_of_json;
    hygiene = pet_json |> member "hygiene" |> to_int;
  }

let pets_of_json pets_json =
  { pets = pets_json |> member "pets" |> to_list |> List.map pet_of_json }

let get_pet pets name = List.find (fun pet -> pet.name = name) pets.pets
let get_health pet = pet.health
let get_gender pet = pet.gender
let get_hunger pet = pet.hunger
let get_hygiene pet = pet.hygiene
let get_description pet = pet.description
let get_name pet = pet.name
let get_bad_foods pet = pet.bad_foods

let get_bad_food (pet : pet) name : food =
  List.find (fun (food : food) -> food.fname = name) pet.bad_foods

let get_bad_food_effect food = food.health_effect
let get_bad_food_name (food : food) : string = food.fname
let get_good_foods pet = pet.good_foods

let get_good_food (pet : pet) name : food =
  List.find (fun (food : food) -> food.fname = name) pet.good_foods

let get_good_food_effect food = food.health_effect
let get_good_food_name (food : food) : string = food.fname
let get_foods pet = List.flatten [ pet.bad_foods; pet.good_foods ]
let get_food_hunger_effect food = food.hunger_effect

let update_pet_hunger pet food_value =
  let current_pet_hunger = get_hunger pet in
  if current_pet_hunger <= 0 then raise AlreadyFull
  else
    {
      name = get_name pet;
      gender = get_gender pet;
      description = get_description pet;
      health = get_health pet;
      hunger = max (current_pet_hunger - food_value) 0;
      bad_foods = pet.bad_foods;
      good_foods = pet.good_foods;
      hygiene = get_hygiene pet;
    }

let update_pet_health pet food_health_effect =
    if pet.health >= 100 then raise AlreadyHealthy;
    {
      name = get_name pet;
      gender = get_gender pet;
      description = get_description pet;
      health = min (get_health pet + food_health_effect) 100;
      hunger = get_hunger pet;
      bad_foods = pet.bad_foods;
      good_foods = pet.good_foods;
      hygiene = get_hygiene pet;
    }

let update_pet_hygiene pet hygiene_effect =
  if pet.hygiene >= 100 then raise AlreadyClean
  else
  { pet with hygiene = min (get_hygiene pet + hygiene_effect) 100 }
