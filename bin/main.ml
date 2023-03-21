(** [play_game f] starts the adventure in file [f]. *)

open Stdlib
open Game
open Pet

(* let play_game f = Stdlib.print_string (Pet.getDescription (Pet.pet_of_json
   (Yojson.Basic.from_file f))) *)

let data_dir_prefix = "data" ^ Filename.dir_sep

let play_game_helper1 f n b =
  Stdlib.print_string
    (Pet.get_description
       (Pet.get_pet (Pet.pets_of_json (Yojson.Basic.from_file f)) n))

let play_game_helper2 f n b =
  Pet.get_pet (Pet.pets_of_json (Yojson.Basic.from_file f)) n

let play_game f b =
  print_string "Enter Name of pet > ";
  match read_line () with
  | read_line -> (
      play_game_helper1 f "null" read_line;
      let pet = play_game_helper2 f "null" read_line in
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\n\nAmy forgot to feed the cat.\n";
      print_endline "Do you want to feed the cat, type 'yes' to feed.";
      print_endline ("DEBUG 1: " ^ string_of_int (Pet.get_hunger pet));
      print_string "> ";
      match Stdlib.read_line () with
      | "yes" -> print_int (Pet.get_hunger (Pet.update_pet_hunger pet 5))
      | _ ->
          ();
          print_endline
            ("DEBUG 2: "
            ^ string_of_int (Pet.get_hunger (Pet.update_pet_hunger pet 5))))

(** [main ()] prompts for the game to play, then starts it. *)

let rec main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to tomagachi game engine.\n";
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game (data_dir_prefix ^ file_name ^ ".json") "null"

(* Execute the game engine. *)

let () = main ()

(* let () = ANSITerminal.print_string [ ANSITerminal.red ] "\n\nAmy forgot to
   feed the cat.\n"; print_endline "Do you want to feed the cat, type 'yes' to
   feed."; print_string "> "; match read_line () with | "yes" -> pet | _ ->
   () *)
