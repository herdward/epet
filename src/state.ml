type state = {
  hunger : int;
  health : int;
}

let init_state pet =
  { hunger = Pet.get_hunger pet; health = Pet.get_health pet }

type result =
  | Legal of state
  | Illegal

let feed fd pet st =
  (*updates the pet's parameters based on feeding*)
  try
    let new_hunger_level = st.hunger - 5 in
    Legal { hunger = new_hunger_level; health = st.health }
  with Pet.AlreadyFull -> Illegal

(* so using state.ml from A2 for inspiration, essentially what we need to do is
   attempt to try out some function, (probably attempt to feed) and then if it
   works then we output a Legal thing that looks like our state record and
   otherwise illegal*)
