(* Note: You may introduce new code anywhere in this file. *)

type object_phrase = string list

type command =
  | Feed of object_phrase
  | Clean of object_phrase
  | Quit

(* notes if get an error or something about unbound constructor, then be sure to
   mirror the changes for type command in both command.mli and command.ml*)
exception Empty
exception Malformed

(* what does go of object phrase even mean?*)

(* go is a variant, quit is a variant *)
(* so will need to check whether the first element is a verb*)

(* can do this with nested pattern matching *)

let parse str =
  let str = String.trim str in
  let string_element_list = String.split_on_char ' ' str in
  let string_element_list =
    List.filter (fun word -> word <> "") string_element_list
  in
  let string_element_list =
    List.map String.lowercase_ascii string_element_list
  in
  let string_element_list = List.map String.trim string_element_list in
  match string_element_list with
  | [] -> raise Empty
  | h :: t -> (
      match h with
      | "feed" -> if t = [] then raise Malformed else Feed t
      | "clean" -> if t = [] then raise Malformed else Clean t
      | "quit" -> if t <> [] then raise Malformed else Quit
      | _ -> raise Malformed)
