open Pet

type state = {
  current_pet : pet option;
  pet_name : string option;
  pet_hunger : int option;
  pet_health : int option;
}

let init_state =
  {
    current_pet = None;
    pet_name = Some "";
    pet_hunger = None;
    pet_health = None;
  }

type result =
  | Legal of state
  | Illegal

let feed food pet st =
  match st.pet_hunger with
  | None -> Illegal (* the pet is not hungry *)
  | Some hunger -> (
      (* for now we can have food just be an int but later on it could be a type
         that we can access the .saturation value from *)
      try
        let new_hunger_level = hunger - food in
        Legal
          {
            current_pet = Some pet;
            pet_name = st.pet_name;
            pet_hunger = Some new_hunger_level;
            pet_health = st.pet_health;
          }
      with Pet.AlreadyFull -> Illegal)

(* so using state.ml from A2 for inspiration, essentially what we need to do is
   attempt to try out some function, (probably attempt to feed) and then if it
   works then we output a Legal thing that looks like our state record and
   otherwise illegal*)

let get_pet_name (st : state) : string =
  match st.pet_name with
  | None -> ""
  | Some x -> x

let get_health = function
  | Legal st -> st.pet_health
  | Illegal -> Some 0

let get_hunger = function
  | Legal st -> st.pet_hunger
  | Illegal -> Some 0

let set_pet_name = function
  | Legal st -> st.pet_name
  | Illegal -> Some ""

let set_health = function
  | Legal st -> st.pet_health
  | Illegal -> Some 0

let set_hunger = function
  | Legal st -> st.pet_hunger
  | Illegal -> Some 0
