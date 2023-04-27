(** Representation of player data.

    This module represents the data stored in the player data JSON file,
    including player name, amount of coins, time, etc.. It handles loading of
    that data from JSON as well as querying the data. *)

type player
(** The abstract type of values representing the player. *)

val player_from_json : Yojson.Basic.t -> player
(** [player_from_json j] is the player representaion from [j]. Requires: [j] is
    a valid player JSON data file. *)

val player_name : player -> string
(** [player_name j] is the string value of the player name from [j]. Requires:
    [j] is a valid player representation. *)

val time_to_string : player -> string
(** [time_to_string j] is the string representaion of the time in player [j].
    Requires: [j] is a valid player representation. *)

val date_to_string : player -> string
(** [date_to_string j] is the string representaion of the current mm/dd/yyyy
    from player [j]. Requires: [j] is a valid player representation. *)

val update_player_date : player -> player
(** [update_player_date j] returns an updated player state by changing the day
    by 1, and if necessary the month changes as well from player [j]. Requires:
    [j] is a valid player representation. *)

val player_coins_total : player -> int
(** [player_coins_total j] returns the int amount of total coins in player [j].
    Requires: [j] is a valid player representation. *)

val player_silver_total : player -> int
(** [player_silver_total j] returns the int amount of silver coins in player
    [j]. Requires: [j] is a valid player representation. *)

val update_player_time : player -> player
(** [update_player_time j] returns the an updated player state by changing the
    time. If time is morning, it is changed to afternoon. If afternoon, time is
    changed to evening. If time is evening, time is changed to morning.
    Requires: [j] is a valid player representation. *)
