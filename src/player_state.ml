exception Invalid_month of string
exception Invalid_time of string

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
  pet_state: State.state option
}





let init_state = {
  name  = None;
  coins = { gold_amount = 30; silver_amount = 10; total_coin = 40 };
  date =
    {
      month_name = "Jan";
      month_int = 1;
      day_number = 1;
      year = 2020;
      time = Morning;
    };
  pet_state =Some  State.init_state

}
let print_player_info player= 
  ANSITerminal.print_string [ ANSITerminal.green ] "Player Info:"
let print_player_name player =
  ANSITerminal.print_string [ ANSITerminal.green ] "Name: ";
  match player.name with 
  | None -> ANSITerminal.print_string [ ANSITerminal.green ] "None"
  | Some name -> ANSITerminal.print_string [ ANSITerminal.green ] name;
  print_endline ""
let print_player_coins player =
  ANSITerminal.print_string [ ANSITerminal.green ] "Coins: ";
  ANSITerminal.print_string [ ANSITerminal.green ] (string_of_int player.coins.total_coin);
  print_endline ""

let print_player_state player = 
  print_player_info player;
  print_player_name player;
  print_player_coins player
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
  {
    gold_amount =
      j |> Yojson.Basic.Util.member "silver coins" |> Yojson.Basic.Util.to_int;
    silver_amount =
      j |> Yojson.Basic.Util.member "gold coins" |> Yojson.Basic.Util.to_int;
    total_coin =
      j |> Yojson.Basic.Util.member "total coins" |> Yojson.Basic.Util.to_int;
  }

let player_from_json (j : Yojson.Basic.t) =
  {
    name =
      Some( j |> Yojson.Basic.Util.member "player_name" |> Yojson.Basic.Util.to_string);
    coins = coin_of_json j;
    date = date_of_json j;
    pet_state = None;
  }

let player_name p = p.name
let player_coins_total coin = coin.coins.total_coin
let player_gold_total coin = coin.coins.gold_amount
let player_silver_total coin = coin.coins.silver_amount

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

let update_time time =
  match time with
  | Morning -> Afternoon
  | Afternoon -> Evening
  | Evening -> Afternoon

let update_month_int date =
  if date.month_int < 12 then 1 else date.month_int + 1

let update_player_date player =
  if player.date.day_number < 30 then
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
    }
  else
    {
      name = player.name;
      coins = player.coins;
      date =
        {
          month_name = month_name (update_month_int player.date);
          month_int = update_month_int player.date;
          day_number = player.date.day_number;
          year = player.date.year;
          time = player.date.time;
        };
        pet_state = player.pet_state;
    }

let update_player_time player =
  {
    name = player.name;
    coins = player.coins;
    date =
      {
        month_name = player.date.month_name;
        month_int = player.date.month_int;
        day_number = player.date.day_number + 1;
        year = player.date.year;
        time = update_time player.date.time;
      };
      pet_state = player.pet_state;
  }
