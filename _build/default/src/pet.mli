(** Representation of static pets data.


    This module represents the data stored in pet files, including all the pets'
    attributes, like health, hunger, description, name, etc.. It handles loading
    of that data from JSON as well as querying the data.


    For examples, the specifications in this interface reference the example
    "Sample" pet list found in [data/samplejson.json]. *)


    type pets
    (** The abstract type of values representing pets. *)
    
    
    type pet
    (** Values of this type have the following attributes: name, gender,
        description, health, and hunger. *)
    
    
    val pet_of_json : Yojson.Basic.t -> pet
    (** [pet_of__json j] is a representation of a pet from [j]. Requires: [j] is a
        valid JSON representation of pets. *)
    
    
    val pets_of_json : Yojson.Basic.t -> pets
    (** [pets_of__json j] is the list of pets that [j] represents. Requires: [j] is
        a valid JSON representation of pets. *)
    
    
    val get_pet : pets -> string -> pet
    (** [get_pet p n] is a pet with name [n] in the list of pets [p]. *)
    
    
    val getHealth : pet -> int
    (** [getHealth p] is the health of pet [p]. *)
    
    
    val getHunger : pet -> int
    (** [getHunger p] is the hunger of pet [p]. *)
    
    
    val getDescription : pet -> string
    (** [getDescription p] is the description of pet [p]. *)
    
    
    val getName : pet -> string
    (** [getName p] is the name of pet [p]. *)
    
    
    