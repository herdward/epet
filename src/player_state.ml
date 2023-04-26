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
  day_number : int;
  year : int;
  time : time;
}

type player = {
  name : string;
  coins : coin;
  time : time;
  date : date;
}

let date_info_from_json j n =
  int_of_string
    (List.nth
       (String.split_on_char ' '
          (j |> Yojson.Basic.Util.member "date" |> Yojson.Basic.Util.to_string))
       n)

let get_month_name i =
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
    month_name = get_month_name (date_info_from_json j 0);
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
      j |> Yojson.Basic.Util.member "player_name" |> Yojson.Basic.Util.to_string;
    coins = coin_of_json j;
    time = time_from_json j;
    date = date_of_json j;
  }

let player_name p = p.name
let coins_in_inventory p = p.coins.total_coin
let current_time p = p.time
let current_date p = p.date
