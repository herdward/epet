(** Representation of player data.

    This module represents the data stored in the player data JSON file,
    including player name, amount of coins, time, etc.. It handles loading of
    that data from JSON as well as querying the data. *)
    type time =
    | Morning
    | Afternoon
    | Evening
  
  type coin = {
    gold_amount : int;
    silver_amount : int;
    total_coin : int;
  }
  
  type date = {
    month_name : string;
    month_int : int;
    day_number : int;
    year : int;
    time : time;
  }
type player_state = {
        name : string option;
        coins : coin;
        date : date;
        pet_state : State.state option
      }

    

val player_from_json : Yojson.Basic.t -> player_state
(** [player_from_json j] is the player representaion from [j]. Requires: [j] is
    a valid player JSON data file. *)

val player_name : player_state -> string option
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

val player_silver_total : player_state -> int
(** [player_silver_total j] returns the int amount of silver coins in player
    [j]. Requires: [j] is a valid player representation. *)

val update_player_time : player_state -> player_state
(** [update_player_time j] returns the an updated player state by changing the
    time. If time is morning, it is changed to afternoon. If afternoon, time is
    changed to evening. If time is evening, time is changed to morning.
    Requires: [j] is a valid player representation. *)

val init_state : player_state
(** [init_state] is the initial player state. *)

val print_player_info : 'a -> unit 

(** [print_player_info ()] prints the player's name, and coins. *)

val print_player_state : player_state -> unit
(** [print_player_state j] prints the player's name, coins, date, and time from
    player [j]. Requires: [j] is a valid player representation. *)