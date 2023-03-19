(** [play_game f] starts the adventure in file [f]. *)

open Stdlib
open Game
open Pet

(* let play_game f = Stdlib.print_string (Pet.getDescription (Pet.pet_of_json
   (Yojson.Basic.from_file f))) *)

let data_dir_prefix = "data" ^ Filename.dir_sep

let play_game_helper1 f n =
  Stdlib.print_string
    (Pet.getDescription
       (Pet.get_pet (Pet.pets_of_json (Yojson.Basic.from_file f)) n))

let play_game f =
  print_string "Enter Name of pet > ";
  match read_line () with
  | read_line -> play_game_helper1 f read_line

(** [main ()] prompts for the game to play, then starts it. *)

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to tomagachi game engine.\n";
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game (data_dir_prefix ^ file_name ^ ".json")

(* Execute the game engine. *)

let () = main ()
