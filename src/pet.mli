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

val food_amount : food -> int
(** [food_amount f] is the amount of food [f].*)

val get_food_hunger_effect : food -> int
(** [get_food_hunger_effect f] f is the hunger effect of food [f].*)

val get_bad_food : pet -> string -> food
val get_bad_food_effect : food -> int
val get_bad_food_name : food -> string
val get_good_foods : pet -> food list
val get_good_food : pet -> string -> food
val get_good_food_name : food -> string
val get_good_food_effect : food -> int
val update_pet_health : pet -> int -> pet

val update_pet_hygiene : pet -> int -> pet
(** [update_pet_health p health_value] gives a pet with its health attribute
    updated, to the maximum of (0, current health + health_value.) Raises
    [AlreadyDead pet] if [pet]'s health level is already 0. This is equivalent
    to [pet]'s health level being 100. *)

val update_pet_hunger : pet -> int -> pet
(** [update_pet_hunger p food_value] gives a pet with its hunger attribute
    updated, to the maximum of (0, current hunger - food_value.) Raises
    [AlreadyFull pet] if [pet]'s fullness level is already 100. This is
    equivalent to [pet]'s hunger level being 0. *)

val update_pet_good_food : food -> pet -> pet
val update_pet_bad_food : food -> pet -> pet
val food_equality : food -> food -> bool
val update_food_amount : food -> food
