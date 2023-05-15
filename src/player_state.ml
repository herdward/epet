exception Invalid_month of string
exception Invalid_time of string

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

let init_state =
  {
    name = "Unamed Player";
    coins = 40;
    date =
      {
        month_name = "Jan";
        month_int = 1;
        day_number = 1;
        year = 2020;
        time = Morning;
      };
    pet_state = Some State.init_state;
    numer_of_actions = 0;
  }

let date_info_from_json j n =
  int_of_string
    (List.nth
       (String.split_on_char ' '
          (j |> Yojson.Basic.Util.member "date" |> Yojson.Basic.Util.to_string))
       n)

let month_name i =
  match i with
  | 1 -> "Jan"
  | 2 -> "Feb"
  | 3 -> "Mar"
  | 4 -> "Apr"
  | 5 -> "May"
  | 6 -> "June"
  | 7 -> "July"
  | 8 -> "Aug"
  | 9 -> "Sep"
  | 10 -> "Oct"
  | 11 -> "Nov"
  | 12 -> "Dec"
  | _ ->
      raise (Invalid_argument (string_of_int i ^ "is not a valid month number"))

let time_from_json j =
  match j |> Yojson.Basic.Util.member "time" |> Yojson.Basic.Util.to_string with
  | "Morning" -> Morning
  | "Afternoon" -> Afternoon
  | "Evening" -> Evening
  | _ -> raise (Invalid_month "Invalid time from JSON")

let date_of_json j =
  {
    month_name = month_name (date_info_from_json j 0);
    month_int = date_info_from_json j 0;
    day_number = date_info_from_json j 1;
    year = date_info_from_json j 2;
    time = time_from_json j;
  }

let coin_of_json j =
  j |> Yojson.Basic.Util.member "total_coins" |> Yojson.Basic.Util.to_int

let player_from_json (j : Yojson.Basic.t) =
  {
    name =
      j |> Yojson.Basic.Util.member "player_name" |> Yojson.Basic.Util.to_string;
    coins = coin_of_json j;
    date = date_of_json j;
    pet_state = None;
    numer_of_actions = 0;
  }

let player_name p = p.name
let player_coins_total coin = coin.coins

let time_to_string player =
  match player.date.time with
  | Morning -> "Morning"
  | Afternoon -> "Afternoon"
  | Evening -> "Evening"

let date_to_string player =
  player.date.month_name ^ " "
  ^ string_of_int player.date.day_number
  ^ " "
  ^ string_of_int player.date.year

let print_player_info player =
  ANSITerminal.print_string [ ANSITerminal.green ] "Player Name: ";
  match player.name with
  | name ->
      ANSITerminal.print_string [ ANSITerminal.green ] (name ^ " |");
      ANSITerminal.print_string [ ANSITerminal.green ]
        (" Coins: " ^ string_of_int player.coins);
      ANSITerminal.print_string [ ANSITerminal.green ]
        (" | Date : " ^ date_to_string player);
      ANSITerminal.print_string [ ANSITerminal.green ]
        (" | Time : " ^ time_to_string player);
      print_endline ""

let update_time time =
  match time with
  | Morning -> Afternoon
  | Afternoon -> Evening
  | Evening -> Afternoon

let get_actions player = player.numer_of_actions
let should_year_be_updated a : bool = if a + 1 >= 13 then true else false
let should_month_be_updated a : bool = if a + 1 > 30 then true else false

let update_player_date (player : player_state) =
  if
    (should_month_be_updated player.date.month_int
    && should_year_be_updated player.date.year)
    == false
  then
    {
      name = player.name;
      coins = player.coins;
      date =
        {
          month_name = player.date.month_name;
          month_int = player.date.month_int;
          day_number = player.date.day_number + 1;
          year = player.date.year;
          time = player.date.time;
        };
      pet_state = player.pet_state;
      numer_of_actions = player.numer_of_actions;
    }
  else if
    should_month_be_updated player.date.month_int
    && not (should_year_be_updated player.date.year)
  then
    {
      name = player.name;
      coins = player.coins;
      date =
        {
          month_name = month_name (player.date.month_int + 1);
          month_int = player.date.month_int + 1;
          day_number = 0;
          year = player.date.year;
          time = player.date.time;
        };
      pet_state = player.pet_state;
      numer_of_actions = player.numer_of_actions;
    }
  else
    {
      name = player.name;
      coins = player.coins;
      date =
        {
          month_name = "Jan";
          month_int = player.date.month_int;
          day_number = 1;
          year = player.date.year + 1;
          time = player.date.time;
        };
      pet_state = player.pet_state;
      numer_of_actions = player.numer_of_actions;
    }

let update_player_time player =
  {
    name = player.name;
    coins = player.coins;
    date =
      {
        month_name = player.date.month_name;
        month_int = player.date.month_int;
        day_number = player.date.day_number;
        year = player.date.year;
        time = update_time player.date.time;
      };
    pet_state = player.pet_state;
    numer_of_actions = player.numer_of_actions;
  }

let update_player_action player =
  {
    name = player.name;
    coins = player.coins;
    date =
      {
        month_name = player.date.month_name;
        month_int = player.date.month_int;
        day_number = player.date.day_number;
        year = player.date.year;
        time = update_time player.date.time;
      };
    pet_state = player.pet_state;
    numer_of_actions = player.numer_of_actions + 1;
  }

let update_state_from_pet player_state pet_state =
  match player_state with
  | state -> { state with pet_state }
