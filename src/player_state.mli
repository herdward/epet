(** Representation of player data.

    This module represents the data stored in the player data JSON file,
    including player name, amount of coins, time, etc.. It handles loading of
    that data from JSON as well as querying the data. *)
type time =
  | Morning
  | Afternoon
  | Evening

type coin = int

type date = {
  month_name : string;
  month_int : int;
  day_number : int;
  year : int;
  time : time;
}

type player_state = {
  name : string;
  coins : coin;
  date : date;
  pet_state : State.state option;
  numer_of_actions : int;
}

val player_from_json : Yojson.Basic.t -> player_state
(** [player_from_json j] is the player representaion from [j]. Requires: [j] is
    a valid player JSON data file. *)

val player_name : player_state -> string
(** [player_name j] is the string value of the player name from [j]. Requires:
    [j] is a valid player representation. *)

val time_to_string : player_state -> string
(** [time_to_string j] is the string representaion of the time in player [j].
    Requires: [j] is a valid player representation. *)

val date_to_string : player_state -> string
(** [date_to_string j] is the string representaion of the current mm/dd/yyyy
    from player [j]. Requires: [j] is a valid player representation. *)

val update_player_date : player_state -> player_state
(** [update_player_date j] returns an updated player state by changing the day
    by 1, and if necessary the month changes as well from player [j]. Requires:
    [j] is a valid player representation. *)

val player_coins_total : player_state -> int
(** [player_coins_total j] returns the int amount of total coins in player [j].
    Requires: [j] is a valid player representation. *)

val update_player_time : player_state -> player_state
(** [update_player_time j] returns the an updated player state by changing the
    time. If time is morning, it is changed to afternoon. If afternoon, time is
    changed to evening. If time is evening, time is changed to morning.
    Requires: [j] is a valid player representation. *)

val init_state : player_state
(** [init_state] is the initial player state. *)

val print_player_info : player_state -> unit
(** [print_player_info] prints all the important information about the player
    state to the terminal such as the player's name, current date, time, and
    coin amount. *)

val update_state_from_pet : player_state -> State.state option -> player_state
(** [update_state_from_pet j s] returns an updated player state by updating the
    pet state to [s] from player [j]. Requires: [j] is a valid player
    representation. *)

val get_actions : player_state -> int
(** [get_actions player_state] returns the amount of actions the player has used
    such as feed, clean since the game has began. This is used for updating the
    time / date in the game loop *)

val update_player_action : player_state -> player_state
(** [update_player_action player_state] returns an updated [player_state] with
    the number of actions for [player_state] increased by one. *)
