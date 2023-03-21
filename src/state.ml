type state = {
  hunger : int;
  health : int; (*currency : int;*)
}

let init_state pet =
  { hunger = Pet.get_hunger pet; health = Pet.get_health pet }

type result =
  | Legal of state
  | Illegal

let feed food pet st =
  (* for now we can have food just be an int but later on it could be a type
     that we can access the .saturation value from *)
  try
    let new_hunger_level = st.hunger - food in
    Legal { hunger = new_hunger_level; health = st.health }
  with Pet.AlreadyFull -> Illegal

(* so using state.ml from A2 for inspiration, essentially what we need to do is
   attempt to try out some function, (probably attempt to feed) and then if it
   works then we output a Legal thing that looks like our state record and
   otherwise illegal*)
