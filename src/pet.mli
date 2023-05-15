(** Representation of static pets data.

    This module represents the data stored in pet files, including all the pets'
    attributes, like health, hunger, description, name, etc.. It handles loading
    of that data from JSON as well as querying the data.

    For examples, the specifications in this interface reference the example
    "Sample" pet list found in [data/samplejson.json]. *)

exception AlreadyFull
(** Raised when the pet's hunger is already 0, and an attempt at raising it
    through actions such as feeding occurred.*)

exception AlreadyHealthy
(** Raised when the pet's health is already 100, and an attempt at raising it
    through actions such as feeding occurred.*)

exception AlreadyClean
(** Raised when the pet's hygiene is already 100, and an attempt at raising it
    through actions such as cleaning occurred.*)

type pets
(** The abstract type of values representing pets. *)

type food
(** The abstract type of values representing foods. *)

type pet
(** Values of this type have the following attributes: name, gender,
    description, health, hunger, and bad foods. *)

val pet_of_json : Yojson.Basic.t -> pet
(** [pet_of__json j] is a representation of a pet from [j]. Requires: [j] is a
    valid JSON representation of pets. *)

val pets_of_json : Yojson.Basic.t -> pets
(** [pets_of__json j] is the list of pets that [j] represents. Requires: [j] is
    a valid JSON representation of pets. *)

val get_pet : pets -> string -> pet
(** [get_pet p n] is a pet with name [n] in the list of pets [p]. *)

val get_health : pet -> int
(** [getHealth p] is the health of pet [p]. *)

val get_hygiene : pet -> int
(** [getHygiene p] is the hygiene of pet [p]. *)

val get_hunger : pet -> int
(** [getHunger p] is the hunger of pet [p]. *)

val get_gender : pet -> string
(** [get_gender p ] is the gender of the pet [p]*)

val get_description : pet -> string
(** [getDescription p] is the description of pet [p]. *)

val get_name : pet -> string
(** [getName p] is the name of pet [p]. *)

val get_bad_foods : pet -> food list
(** [get_bad_foods p] is the list of bad foods for pet [p]. *)

val get_good_foods : pet -> food list
(** [get_good_foods p] is the list of good foods for pet [p]. *)

val food_amount : food -> int
(** [food_amount f] is the amount of food [f].*)

val get_food_hunger_effect : food -> int
(** [get_food_hunger_effect f] is the hunger effect of food [f].*)

val get_bad_food_name : food -> string
(** [get_bad_food_name f] is the fname of food [f].*)

val get_good_food_name : food -> string
(** [get_good_food_name f] is the fname of food [f].*)

val get_bad_food : pet -> string -> food
(** [get_bad_food p n] is the food with name [n] in the list of bad foods for
    pet [p].*)

val get_good_food : pet -> string -> food
(** [get_good_food p n] is the food with name [n] in the list of good foods for
    pet [p].*)

val get_bad_food_effect : food -> int
(** [get_bad_food_effect f] is the bad health effect of food [f]. *)

val get_good_food_effect : food -> int
(** [get_good_food_effect f] is the good health effect of food [f]. *)

val update_pet_health : pet -> int -> pet
(** [update_pet_health p health_value] gives a pet with its health attribute
    updated, to the minimum of (100, current health + health_value.) Raises
    [AlreadyHealthy pet] if [p]'s health level is already 100. *)

val update_pet_hygiene : pet -> int -> pet
(** [update_pet_hygiene p hygiene_effect] gives a pet with its hygiene attribute
    updated, to the minimum of (100, current hygiene + hygiene_effect.) Raises
    [AlreadyClean pet] if [p]'s hygiene level is already 100. *)

val update_pet_hunger : pet -> int -> pet
(** [update_pet_hunger p food_value] gives a pet with its hunger attribute
    updated, to the maximum of (0, current hunger - food_value.) Raises
    [AlreadyFull pet] if [p]'s fullness level is already 100. This is equivalent
    to [p]'s hunger level being 0. *)

val food_equality : food -> food -> bool
(** [food_equality f1 f2] checks if food [f1] and food [f2] are equivalent by
    checking if their names, health_effect, hunger_effect, and amount attributes
    are the same. Returns true if so, and false otherwise. *)

val update_pet_good_food : food -> pet -> pet
val update_pet_bad_food : food -> pet -> pet

val update_food_amount : food -> food
(** [update_food_amount f] is food [f], but with its amount attribute changed to
    be amount - 1. *)
val food_amount : food -> int
