(** [play_game f] starts the adventure in file [f]. *)

open Stdlib
open Game
open Pet
open Player_state
open Yojson.Basic

let data_dir_prefix = "data" ^ Filename.dir_sep


let init_player_state_from_json =
  player_from_json
    (Yojson.Basic.from_file (data_dir_prefix ^ "player_state_test" ^ ".json")) 

let getOption input =
  match input with
  | Some a -> a
  | None -> failwith "getOption failed"


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
  let pet_hygiene = get_hygiene pet in
  let pet_info_str =
    Printf.sprintf "\nPET NAME : %s | HEALTH : %d | HUNGER : %d | HYGIENE: %d\n"
      pet_name pet_health pet_hunger pet_hygiene
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
    ^ " " ^ "health" ^ "\n");
  updated_pet

let food_result food pet =
  if check_if_bad_food food (Pet.get_bad_foods pet) then
    bad_food_result food pet
  else if check_if_good_food food (Pet.get_good_foods pet) then
    good_food_result food pet
  else (
    ANSITerminal.print_string
      [ ANSITerminal.red; ANSITerminal.Bold ]
      ("\nTHAT FOOD IS NOT AVAILABLE, " ^ Pet.get_name pet
     ^ " was not affected\n");
    pet)

let feedencounter (pet : pet) : pet =
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
  | input_food ->
      let updatedpet = food_result input_food pet in
      print_pet_info updatedpet |> ignore;
      updatedpet (* this is to make sure that the UI gets updated*)

let cleanencounter (pet : pet) : pet =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n How would you like to clean your pet? \n";
  ANSITerminal.print_string [ ANSITerminal.blue ] "\n 1. Take a bath \n";
  ANSITerminal.print_string [ ANSITerminal.blue ] "\n 2. Brush their fur \n";
  ANSITerminal.print_string [ ANSITerminal.blue ] "\n 3. Quit \n";
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n Type 1 or 2 to select your choice \n";
  match Stdlib.read_line () with
  | "1" ->
      let updatedpet = Pet.update_pet_hygiene pet 25 in
      ANSITerminal.print_string [ ANSITerminal.green ]
        "\n Your pet is now cleaner! \n";
      print_pet_info updatedpet |> ignore;
      updatedpet
  | "2" ->
      let updatedpet = Pet.update_pet_hygiene pet 10 in
      ANSITerminal.print_string [ ANSITerminal.green ]
        "\n Your pet is now cleaner! \n";
      print_pet_info updatedpet |> ignore;
      updatedpet
  | "3" -> Unix._exit 0
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\n That is not a valid choice! \n";
      pet

let available_actions = [ "feed"; "clean" ]

(* Define the game state record *)

(* Initialize the game state *)
let init_pet_state = State.init_state
(* the initial state represents the state with no pet chosen yet*)

(* Function to prompt the user to select a pet *)

let rec select_pet (state : State.state) : State.state =
  print_string "\n Please type the name of the pet you would like to check on.";
  print_string "You can currently check on: cat\n> ";
  (* right now this is just hardcoded but may need to change*)
  match Stdlib.read_line () with
  | "quit" -> Unix._exit 0
  | petname -> (
      try
        let pet = Pet.get_pet pet_list petname in
        (* update the interface*)
        (* ask what action want to do, currently the only action is feed*)
        print_pet_info pet |> ignore;
        let new_state =
          {
            state with
            current_pet = Some pet;
            pet_name = Some (Pet.get_name pet);
            pet_health = Some (Pet.get_health pet);
            pet_hunger = Some (Pet.get_hunger pet);
            pet_hygiene = Some (Pet.get_hygiene pet);
          }
        in
        new_state
      with
      | Not_found ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "That pet does not exist. Please try again.";
          select_pet state
      | exc ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "\nAn error occurred while selecting a pet. Please try again.\n";
          select_pet state)
  | exception End_of_file -> exit 0

let rec select_action (state : State.state) : State.state =
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "\nWhat would you like to do? \n";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "\n The available actions are:";
  ANSITerminal.print_string [ ANSITerminal.yellow ] " feed, clean, quit\n";

  match Stdlib.read_line () with
  | action -> (
      match action with
      | "feed" -> begin
          match state.current_pet with
          | None ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                "\nYou must select a pet before you can feed it.";
              select_action state
          | Some pet ->
              let updatedpet = feedencounter pet in
              let new_state =
                {
                  state with
                  current_pet = Some updatedpet;
                  pet_name = Some (Pet.get_name updatedpet);
                  pet_health = Some (Pet.get_health updatedpet);
                  pet_hunger = Some (Pet.get_hunger updatedpet);
                }
              in
              select_action new_state
        end
      | "clean" -> begin
          match state.current_pet with
          | None ->
              ANSITerminal.print_string [ ANSITerminal.red ]
                "\nYou must select a pet before you can clean it.";
              select_action state
          | Some pet ->
              let updatedpet = cleanencounter pet in
              let new_state =
                {
                  state with
                  current_pet = Some updatedpet;
                  pet_name = Some (Pet.get_name updatedpet);
                  pet_health = Some (Pet.get_health updatedpet);
                  pet_hunger = Some (Pet.get_hunger updatedpet);
                  pet_hygiene = Some (Pet.get_hygiene updatedpet);
                }
              in
              select_action new_state
        end
      | "quit" -> Unix._exit 0
      | _ ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "\nThat action does not exist. Please try again.";
          select_action state)
  | exception End_of_file -> exit 0
(* Game loop *)

let rec pet_game_loop (state : State.state)
    (player_state : Player_state.player_state) : State.state =
  Player_state.print_player_state player_state |> ignore;
  if state = init_pet_state then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      "\nYou are currently not checking on any pet.\n";
    pet_game_loop (select_pet state) player_state)
  else (
    ANSITerminal.print_string [ ANSITerminal.red ]
      ("\n\nWelcome to E-Pet Game!\nYou are currently checking on "
     ^ State.get_pet_name state ^ "\n");
    pet_game_loop (select_action state) player_state)

let init_player_state = Player_state.init_state

  
let rec player_game_loop (player_state: Player_state.player_state) : Player_state.player_state = 
  if player_state = init_player_state then 
    begin
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\n\nWelcome to E-Pet Game!\nWhat is your name? .\n";
      match Stdlib.read_line () with
      | input -> let new_state = {player_state with name = Some input} in 
                 player_game_loop new_state
      | exception End_of_file -> exit 0 
    end
  else 
    let old_pet_state = player_state.pet_state in
    match old_pet_state with 
    | None -> let new_pet_state = Some (select_pet init_pet_state) in
              let new_state = {player_state with pet_state = new_pet_state} in
              Player_state.print_player_state new_state;
              player_game_loop new_state
    | Some old_pet_state -> let new_pet_state = pet_game_loop(old_pet_state) player_state in 
    let new_state = {player_state with pet_state =Some  new_pet_state} in
    Player_state.print_player_state new_state;
    player_game_loop new_state

(*game_loop (select_action state)*)
(* If current pet, select action *)

(* Start the game *)
let () = ignore (player_game_loop init_player_state)
(*let () = ignore (pet_game_loop init_pet_state)*)

open Pet
