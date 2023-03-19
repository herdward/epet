(** Representation of dynamic adventure state.

    This module represents the state of an adventure as it is being played,
    including the adventurer's current room, the rooms that have been visited,
    and functions that cause the state to change. *)

(**********************************************************************
 * DO NOT CHANGE THIS FILE
 * It is part of the interface the course staff will use to test your
 * submission.
 **********************************************************************)

type state
(** The abstract type of values representing the game state. *)

val init_state : Pet.pet -> state
(** [init_state a] is the initial state of the game when playing adventure [a].
    In that state the adventurer is currently located in the starting room, and
    they have visited only that room. *)

val current_room_id : Pet.pet -> string
(** [current_room_id st] is the identifier of the room in which the adventurer
    is currently located in state [st]. *)

val visited : Pet.pet -> string list
(** [visited st] is a set-like list of the room identifiers the adventurer has
    visited in state [st]. The adventurer has visited a room [rm] if their
    current room location is or has ever been [rm]. *)

(** The type representing the result of an attempted interaction , such as
    feeding, cleaning, petting, playing, etc. *)
type result =
  | Legal of state
  | Illegal

val feed : string -> Pet.pet -> state -> result
(** [feed food pet st] is the result of attempting to feed the food [food] to
    the pet [pet]

    - If food is the name of a food that the pet can eat then the result is
      [Legal st'], where in [st'] the pet now has updated food meter / health
      meter.

    - Otherwise, the result is [Illegal].

    (Side) Effects: none. In particular, [feed] does not print anything. *)
