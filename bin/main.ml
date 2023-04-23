(** [play_game f] starts the adventure in file [f]. *)

open Stdlib
open Game
open Pet

let data_dir_prefix = "data" ^ Filename.dir_sep

let pet_list =
  Pet.pets_of_json
    (Yojson.Basic.from_file (data_dir_prefix ^ "samplejson" ^ ".json"))

let rec check_if_food food petfoodlist =
  match petfoodlist with
  | [] -> false
  | h :: t ->
      if Pet.get_good_food_name h = food || Pet.get_bad_food_name h = food then
        true
      else check_if_food food t

let rec check_if_bad_food food petfoodlist =
  match petfoodlist with
  | [] -> false
  | h :: t ->
      if Pet.get_bad_food_name h = food then true else check_if_bad_food food t

let rec check_if_good_food food petfoodlist =
  match petfoodlist with
  | [] -> false
  | h :: t ->
      if Pet.get_good_food_name h = food then true
      else check_if_good_food food t

let print_pet_info pet =
  let pet_name = get_name pet in
  let pet_health = get_health pet in
  let pet_hunger = get_hunger pet in
  let pet_info_str =
    Printf.sprintf "\nPET NAME : %s | HEALTH : %d | HUNGER : %d\n" pet_name
      pet_health pet_hunger
  in
  ANSITerminal.print_string
    [ ANSITerminal.Bold; ANSITerminal.cyan ]
    pet_info_str

let rec bad_food_result food pet =
  let updated_pet =
    Pet.update_pet_health pet
      (Pet.get_bad_food_effect (Pet.get_bad_food pet food))
  in

  ANSITerminal.print_string [ ANSITerminal.red ]
    ("\n You selected to feed " ^ Pet.get_name pet ^ " with "
    ^ Pet.get_bad_food_name (Pet.get_bad_food pet food));

  print_endline
    ("\nAhh!" ^ " " ^ Pet.get_name pet
   ^ " did not like that! They lost health :(");
  print_string
    ("\n" ^ Pet.get_name pet ^ " lost" ^ " "
    ^ string_of_int (abs (Pet.get_bad_food_effect (Pet.get_bad_food pet food)))
    ^ " " ^ "health");
  updated_pet

let rec good_food_result food pet =
  let updated_pet =
    Pet.update_pet_health pet
      (Pet.get_good_food_effect (Pet.get_good_food pet food))
  in
  ANSITerminal.print_string [ ANSITerminal.green ]
    ("\n You selected to feed " ^ Pet.get_name pet ^ " with "
    ^ Pet.get_good_food_name (Pet.get_good_food pet food));
  print_endline
    ("\nYum! " ^ Pet.get_name pet ^ " loved that! They gained health :)");
  print_string
    ("\n" ^ Pet.get_name pet ^ " gained" ^ " "
    ^ string_of_int (Pet.get_good_food_effect (Pet.get_good_food pet food))
    ^ " " ^ "health");
  updated_pet

let food_result food pet =
  if check_if_bad_food food (Pet.get_bad_foods pet) then
    bad_food_result food pet
  else if check_if_good_food food (Pet.get_good_foods pet) then
    good_food_result food pet
  else pet
(*here should be the application of good_food_result or smth similar*)

let feedencounter (pet : pet) : pet =
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

let feedencounter2 (pet : pet) : pet =
  let good_foods = Pet.get_good_foods pet in
  let bad_foods = Pet.get_bad_foods pet in
  let good_foods_names = List.map Pet.get_good_food_name good_foods in
  let bad_foods_names = List.map Pet.get_bad_food_name bad_foods in
  let available_food_names = good_foods_names @ bad_foods_names in

  ANSITerminal.print_string [ ANSITerminal.red ]
    ("\nYour available foods are "
    ^ String.concat ", " available_food_names
    ^ "\n ");
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
      (* update the interface*)
      let pet = feedencounter pet in

      ignore pet;
      ()

let available_actions = [ "feed" ]

let select_actions () =
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to E-Pet Game!\n";
  print_string "Please type the name of the pet you would like to check on.";
  print_string "\n> ";
  match Stdlib.read_line () with
  | a -> (
      let pet = Pet.get_pet pet_list a in
      (* update the interface*)
      (* ask what action want to do, currently the only action is feed*)
      print_pet_info pet |> ignore;
      ANSITerminal.print_string [ ANSITerminal.red ]
        ("\nYou selected: " ^ Pet.get_name (Pet.get_pet pet_list a) ^ "\n");

      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "\nWhat would you like to do? \n";
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "\n The available actions are: \n";
      ANSITerminal.print_string [ ANSITerminal.yellow ] "\n feed \n";

      match Stdlib.read_line () with
      | "feed" -> print_pet_info (feedencounter2 pet) |> ignore
      | _ -> failwith "Not a valid action")

(* Execute the game engine. *)
let () = select_actions ()

(* let () = select_pet() *)

open Pet
