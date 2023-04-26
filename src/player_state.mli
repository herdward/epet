(** Representation of player data.

    This module represents the data stored in the player data JSON file,
    including player name, amount of coins, time, etc.. It handles loading of
    that data from JSON as well as querying the data. *)

type player
(** The abstract type of values representing the player. *)

type time
(** The abstract type of values representing time. *)

type day
(** The abstract type of values representing days. *)

type coin
(** The abstract type of values representing coins. *)

val player_from_json : Yojson.Basic.t -> player
(** [player_from_json j] is the player representaion from [j]. Requires: [j] is
    a valid player JSON data file. *)

val player_name : player -> string
(** [player_name j] is the string value of the player name from [j]. Requires:
    [j] is a valid player representation. *)

val coins_in_inventory : player -> int
(** [coins_in_inventory j] is the int value of the number of coins the the
    player has from [j]. Requires: [j] is a valid player representation. *)

val current_time : player -> time
(** [current_time j] is the time representaion from player [j]. Requires: [j] is
    a valid player representation. *)

val current_day : player -> day
(** [current_time j] is the day representaion from player [j]. Requires: [j] is
    a valid player representation. *)
