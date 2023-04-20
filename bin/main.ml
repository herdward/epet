(** [play_game f] starts the adventure in file [f]. *)

open Stdlib
open Game
open Pet

let data_dir_prefix = "data" ^ Filename.dir_sep

let pet_list =
  Pet.pets_of_json
    (Yojson.Basic.from_file (data_dir_prefix ^ "samplejson" ^ ".json"))

let rec check_if_bad_food food petfoodlist =
  match petfoodlist with
  | [] -> false
  | h :: t ->
      if Pet.get_bad_food_name h = food then true else check_if_bad_food food t

(** this code doesn't appear to actually make any "edits" to the pet state, will
    have to change, maybe return a pet object with the updated health? And have
    another function to print the result?*)
let bad_food_result food pet =
  ANSITerminal.print_string [ ANSITerminal.red ]
    ("\n You selected to feed " ^ Pet.get_name pet ^ " with "
    ^ Pet.get_bad_food_name (Pet.get_bad_food pet food));
  print_endline
    ("\nAhh!" ^ " " ^ Pet.get_name pet
   ^ " did not like that! They lost health :(");
  print_string
    ("\n" ^ Pet.get_name pet ^ " health is "
    ^ string_of_int
        (Pet.get_health
           (Pet.update_pet_health pet
              (Pet.get_bad_food_effect (Pet.get_bad_food pet food)))))

let food_result food pet =
  if check_if_bad_food food (Pet.get_bad_foods pet) then
    bad_food_result food pet
  else raise (Failure "Not a valid food")
(*here should be the application of good_food_result or smth similar*)

let encounter (pet : pet) : unit =
  print_endline ("Looks like Amy Li forgot to feed " ^ Pet.get_name pet ^ "!!!");
  print_endline
    (Pet.get_name pet ^ " current health: " ^ string_of_int (Pet.get_health pet));
  print_endline
    "\n\
     You have only chocolate and and grapes. Print the name of the food you \
     want to feed your pet in all lowercase";
  print_string "\n> ";
  match Stdlib.read_line () with
  | a -> food_result a pet

let select_pet () =
  (* Print the game prompt *)
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to tomagachi game engine.\n";
  print_string "Please type the name of the pet you would like to check on.";
  print_string "\n> ";
  match Stdlib.read_line () with
  | a ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        ("\nYou selected: " ^ Pet.get_name (Pet.get_pet pet_list a));
      Stdlib.print_endline ("\n" ^ Pet.get_description (Pet.get_pet pet_list a));
      let pet = Pet.get_pet pet_list a in
      encounter pet

(* Execute the game engine. *)

let () = select_pet ()
