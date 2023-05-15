(**Test plan: We had two main approaches to testing our system. The first
   approach were the OUnit tests, which tested the correctness of our functions.
   The second approach was manual testing, which tested the functionality of our
   system. We mainly tested our functions using OUnit, namely its assert_equals
   function. Our tests test the functions in modules Pet and Command, which deal
   with reading information from a JSON file to form the pet, and possible
   commands that the player inputs during the game. These test cases were
   developed so that the information read from JSON was what we expected. Also,
   for functions in the Pet module that changed some of the pet's stats after an
   action, we made sure the change in the pet's stats matched our expectations.
   As for the Command test cases, we developed test cases so that it would cover
   the player's possible inputs during the game, such as mispellings or excess
   white space. Our testing approach demonstrates the correctness of the system
   because it ensures that information is being read correctly from the JSON
   file, as well as any action conducted on the pet produces the correct changes
   in the pet. It also makes sure that commands from the player are being
   interpreted correctly and that the system doesn't quit out because of an
   error.

   With regards to manual testing, we tested the functionality of our system by
   tweaking the initial values of a pet's stats in the JSON file and seeing if
   the expected behavior was shown in the game. For example, if we set the
   initial hunger of the pet to 0, then feeding should not have an effect on the
   hunger. Similarly, if the pet was already healthy, then feeding would not
   increment it past the capacity (100). We also tested to make sure that the
   floor and ceiling of the pet's stats were respected. *)

open OUnit2
open Game
open Pet
open Command

(*Loading files from data directory*)
let data_dir_prefix = "data" ^ Filename.dir_sep
let sample2 = Yojson.Basic.from_file (data_dir_prefix ^ "samplejson2.json")
let sample = Yojson.Basic.from_file (data_dir_prefix ^ "samplejson.json")
let cat = get_pet (pets_of_json sample2) "cat"
let ocat = get_pet (pets_of_json sample) "cat"

let pet_tests =
  [
    ( {|name of cat in samplejson2.json, should be "cat"|} >:: fun _ ->
      assert_equal "cat" (get_name cat) );
    ( {|health of cat in samplejson2.json, should be 100|} >:: fun _ ->
      assert_equal 99 (get_health cat) );
    ( {|gender of cat in samplejson2.json, should be male|} >:: fun _ ->
      assert_equal "male" (get_gender cat) );
    ( {|hunger of cat in samplejson2.json, should be 5|} >:: fun _ ->
      assert_equal 5 (get_hunger cat) );
    ( {|description of samplejson2.json, should be "cat says: 'nice to meet you' in cat language. It looks hungry!"|}
    >:: fun _ ->
      assert_equal
        "cat says: 'nice to meet you' in cat language. It looks hungry!"
        (get_description cat) );
    ( {|a bad food of cat in samplejson2.json with input "chocolate", should be "chocolate"|}
    >:: fun _ ->
      assert_equal "chocolate"
        (get_bad_food_name (get_bad_food cat "chocolate")) );
    ( {|a bad food of cat in samplejson2.json with input "grapes", should be "grapes"|}
    >:: fun _ ->
      assert_equal "grapes" (get_bad_food_name (get_bad_food cat "grapes")) );
    ( {|a bad food of cat in samplejson2.json with input "egg", should be "egg"|}
    >:: fun _ -> assert_equal "egg" (get_bad_food_name (get_bad_food cat "egg"))
    );
    ( {|cat's chocolate effect in samplejson2.json, should be -10|} >:: fun _ ->
      assert_equal (-10) (get_bad_food_effect (get_bad_food cat "chocolate")) );
    ( {|cat's grapes effect in samplejson2.json, should be -8|} >:: fun _ ->
      assert_equal (-8) (get_bad_food_effect (get_bad_food cat "grapes")) );
    ( {|cat's egg effect in samplejson2.json, should be -5|} >:: fun _ ->
      assert_equal (-5) (get_bad_food_effect (get_bad_food cat "egg")) );
    ( {|a good food of cat in samplejson2.json with input "cod", should be "cod"|}
    >:: fun _ ->
      assert_equal "cod" (get_good_food_name (get_good_food cat "cod")) );
    ( {|a good food of cat in samplejson2.json with input "milk", should be "milk"|}
    >:: fun _ ->
      assert_equal "milk" (get_good_food_name (get_good_food cat "milk")) );
    ( {|a good food of cat in samplejson2.json with input "biscuit", should be "biscuit"|}
    >:: fun _ ->
      assert_equal "biscuit" (get_good_food_name (get_good_food cat "biscuit"))
    );
    ( {|a good food of cat in samplejson2.json with input "sausage", should be "sausage"|}
    >:: fun _ ->
      assert_equal "sausage" (get_good_food_name (get_good_food cat "sausage"))
    );
    ( {|cat's cod effect in samplejson2.json, should be 5|} >:: fun _ ->
      assert_equal 5 (get_good_food_effect (get_good_food cat "cod")) );
    ( {|cat's milk effect in samplejson2.json, should be 3|} >:: fun _ ->
      assert_equal 3 (get_good_food_effect (get_good_food cat "milk")) );
    ( {|cat's biscuit effect in samplejson2.json, should be 1|} >:: fun _ ->
      assert_equal 1 (get_good_food_effect (get_good_food cat "biscuit")) );
    ( {|cat's sausage effect in samplejson2.json, should be 2|} >:: fun _ ->
      assert_equal 2 (get_good_food_effect (get_good_food cat "sausage")) );
    ( {|cat's health after being fed cod and biscuit, with health = 99, should
       raise AlreadyHealthy exception|}
    >:: fun _ ->
      assert_raises AlreadyHealthy (fun () ->
          get_health
            (update_pet_health
               (update_pet_health cat
                  (get_good_food_effect (get_good_food cat "cod")))
               (get_good_food_effect
                  (get_good_food
                     (update_pet_health cat
                        (get_good_food_effect (get_good_food cat "cod")))
                     "biscuit")))) );
    ( {|cat's health after being fed cod and chocolate, with health = 99, should
       raise AlreadyHealthy exception|}
    >:: fun _ ->
      assert_raises AlreadyHealthy (fun () ->
          get_health
            (update_pet_health
               (update_pet_health cat
                  (get_good_food_effect (get_good_food cat "cod")))
               (get_bad_food_effect
                  (get_bad_food
                     (update_pet_health cat
                        (get_good_food_effect (get_good_food cat "cod")))
                     "chocolate")))) );
    ( {|cat's health after being fed cod, with health = 99, should be 100|}
    >:: fun _ ->
      assert_equal 100
        (get_health
           (update_pet_health cat
              (get_good_food_effect (get_good_food cat "cod")))) );
    ( {|cat's health after being fed biscuit, with health = 99, should be 100|}
    >:: fun _ ->
      assert_equal 100
        (get_health
           (update_pet_health cat
              (get_good_food_effect (get_good_food cat "biscuit")))) );
    ( {|ocat's health after being fed chocolate, health = 28, should be 18|}
    >:: fun _ ->
      assert_equal 18
        (get_health
           (update_pet_health ocat
              (get_bad_food_effect (get_bad_food ocat "chocolate")))) );
    ( {|ocat's health after being fed grapes, health = 28, should be 20|}
    >:: fun _ ->
      assert_equal 20
        (get_health
           (update_pet_health ocat
              (get_bad_food_effect (get_bad_food ocat "grapes")))) );
    ( {|ocat's health after being fed egg, should be 23|} >:: fun _ ->
      assert_equal 23
        (get_health
           (update_pet_health ocat
              (get_bad_food_effect (get_bad_food ocat "egg")))) );
    ( {|ocat's health after being fed cod, should be 33|} >:: fun _ ->
      assert_equal 33
        (get_health
           (update_pet_health ocat
              (get_good_food_effect (get_good_food ocat "cod")))) );
    ( {|ocat's health after being fed milk, should be 31|} >:: fun _ ->
      assert_equal 31
        (get_health
           (update_pet_health ocat
              (get_good_food_effect (get_good_food ocat "milk")))) );
    ( {|ocat's health after being fed biscuit, should be 29|} >:: fun _ ->
      assert_equal 29
        (get_health
           (update_pet_health ocat
              (get_good_food_effect (get_good_food ocat "biscuit")))) );
    ( {|ocat's health after being fed sausage, should be 30|} >:: fun _ ->
      assert_equal 30
        (get_health
           (update_pet_health ocat
              (get_good_food_effect (get_good_food ocat "sausage")))) );
    ( {|cat's hygiene after being bathed, when cat hygiene = 100, should raise AlreadyClean exception|}
    >:: fun _ ->
      assert_raises AlreadyClean (fun () ->
          get_hygiene (update_pet_hygiene cat 25)) );
    ( {|ocat's hygiene after being brushed, should be 71|} >:: fun _ ->
      assert_equal 71 (get_hygiene (update_pet_hygiene ocat 10)) );
    ( {|ocat's hygiene after being cleaned a lot (testing the min function), should be 100|}
    >:: fun _ -> assert_equal 100 (get_hygiene (update_pet_hygiene ocat 50)) );
    ( {|ocat's hygiene after being cleaned a lot (testing the min function), should be 100|}
    >:: fun _ -> assert_equal 100 (get_hygiene (update_pet_hygiene ocat 39)) );
    ( {|cat's chocolate hunger effect in samplejson2.json, should be 2|}
    >:: fun _ ->
      assert_equal 2 (get_food_hunger_effect (get_bad_food cat "chocolate")) );
    ( {|cat's grapes hunger effect in samplejson2.json, should be 2|}
    >:: fun _ ->
      assert_equal 2 (get_food_hunger_effect (get_bad_food cat "grapes")) );
    ( {|cat's egg hunger effect in samplejson.2json, should be 3|} >:: fun _ ->
      assert_equal 3 (get_food_hunger_effect (get_bad_food cat "egg")) );
    ( {|cat's cod hunger effect in samplejson2.json, should be 5|} >:: fun _ ->
      assert_equal 5 (get_food_hunger_effect (get_good_food cat "cod")) );
    ( {|cat's milk hunger effect in samplejson2.json, should be 4|} >:: fun _ ->
      assert_equal 4 (get_food_hunger_effect (get_good_food cat "milk")) );
    ( {|cat's biscuit hunger effect in samplejson2.json, should be 7|}
    >:: fun _ ->
      assert_equal 7 (get_food_hunger_effect (get_good_food cat "biscuit")) );
    ( {|cat's sausage hunger effect in samplejson2.json, should be 8|}
    >:: fun _ ->
      assert_equal 8 (get_food_hunger_effect (get_good_food cat "sausage")) );
    ( {|ocat's hunger after being fed chocolate, should be 50|} >:: fun _ ->
      assert_equal 50
        (get_hunger
           (update_pet_hunger ocat
              (get_food_hunger_effect (get_bad_food ocat "chocolate")))) );
    ( {|ocat's hunger after being fed grapes, should be 50|} >:: fun _ ->
      assert_equal 50
        (get_hunger
           (update_pet_hunger ocat
              (get_food_hunger_effect (get_bad_food ocat "grapes")))) );
    ( {|ocat's hunger after being fed egg, should be 49|} >:: fun _ ->
      assert_equal 49
        (get_hunger
           (update_pet_hunger ocat
              (get_food_hunger_effect (get_bad_food ocat "egg")))) );
    ( {|ocat's hunger after being fed cod, should be 47|} >:: fun _ ->
      assert_equal 47
        (get_hunger
           (update_pet_hunger ocat
              (get_food_hunger_effect (get_good_food ocat "cod")))) );
    ( {|ocat's hunger after being fed milk, should be 48|} >:: fun _ ->
      assert_equal 48
        (get_hunger
           (update_pet_hunger ocat
              (get_food_hunger_effect (get_good_food ocat "milk")))) );
    ( {|ocat's hunger after being fed biscuit, should be 45|} >:: fun _ ->
      assert_equal 45
        (get_hunger
           (update_pet_hunger ocat
              (get_food_hunger_effect (get_good_food ocat "biscuit")))) );
    ( {|ocat's hunger after being fed sausage, should be 44|} >:: fun _ ->
      assert_equal 44
        (get_hunger
           (update_pet_hunger ocat
              (get_food_hunger_effect (get_good_food ocat "sausage")))) );
    ( {|cat's hunger after being fed cod, should be 0|} >:: fun _ ->
      assert_equal 0
        (get_hunger
           (update_pet_hunger cat
              (get_food_hunger_effect (get_good_food cat "cod")))) );
    ( {|cat's hunger after being fed sausage, should be 0|} >:: fun _ ->
      assert_equal 0
        (get_hunger
           (update_pet_hunger cat
              (get_food_hunger_effect (get_good_food cat "sausage")))) );
    ( {|cat's hunger after being fed cod and sausage, should raise AlreadyFull exception|}
    >:: fun _ ->
      assert_raises AlreadyFull (fun () ->
          update_pet_hunger
            (update_pet_hunger cat
               (get_food_hunger_effect (get_good_food cat "cod")))
            (get_food_hunger_effect
               (get_good_food
                  (update_pet_hunger cat
                     (get_food_hunger_effect (get_good_food cat "cod")))
                  "sausage"))) );
    ( {|food amount of chocolate, should be 3|} >:: fun _ ->
      assert_equal 3 (food_amount (get_bad_food cat "chocolate")) );
    ( {|food amount of grapes, should be 3|} >:: fun _ ->
      assert_equal 3 (food_amount (get_bad_food cat "grapes")) );
    ( {|food amount of egg, should be 3|} >:: fun _ ->
      assert_equal 3 (food_amount (get_bad_food cat "egg")) );
    ( {|food amount of cod, should be 3|} >:: fun _ ->
      assert_equal 3 (food_amount (get_good_food cat "cod")) );
    ( {|food amount of milk, should be 3|} >:: fun _ ->
      assert_equal 3 (food_amount (get_good_food cat "milk")) );
    ( {|food amount of biscuit, should be 3|} >:: fun _ ->
      assert_equal 3 (food_amount (get_good_food cat "biscuit")) );
    ( {|food amount of sausage, should be 3|} >:: fun _ ->
      assert_equal 3 (food_amount (get_good_food cat "sausage")) );
    ( {|food equality of chocolate and chocolate, should be true|} >:: fun _ ->
      assert_equal true
        (food_equality
           (get_bad_food cat "chocolate")
           (get_bad_food cat "chocolate")) );
    ( {|food equality of chocolate and grapes, should be false|} >:: fun _ ->
      assert_equal false
        (food_equality
           (get_bad_food cat "chocolate")
           (get_bad_food cat "grapes")) );
    ( {|food equality of milk and milk, should be true|} >:: fun _ ->
      assert_equal true
        (food_equality (get_good_food cat "milk") (get_good_food cat "milk")) );
    ( {|food equality of milk and biscuit, should be false|} >:: fun _ ->
      assert_equal false
        (food_equality (get_good_food cat "milk") (get_good_food cat "biscuit"))
    );
    ( {|update_food_amount for chocolate, food_amount should be 2|} >:: fun _ ->
      assert_equal 2
        (food_amount (update_food_amount (get_bad_food cat "chocolate"))) );
    ( {|update_food_amount for grapes, food_amount should be 2|} >:: fun _ ->
      assert_equal 2
        (food_amount (update_food_amount (get_bad_food cat "grapes"))) );
    ( {|update_food_amount for egg, food_amount should be 2|} >:: fun _ ->
      assert_equal 2 (food_amount (update_food_amount (get_bad_food cat "egg")))
    );
    ( {|update_food_amount for cod, food_amount should be 2|} >:: fun _ ->
      assert_equal 2
        (food_amount (update_food_amount (get_good_food cat "cod"))) );
    ( {|update_food_amount for milk, food_amount should be 2|} >:: fun _ ->
      assert_equal 2
        (food_amount (update_food_amount (get_good_food cat "milk"))) );
    ( {|update_food_amount for biscuit, food_amount should be 2|} >:: fun _ ->
      assert_equal 2
        (food_amount (update_food_amount (get_good_food cat "biscuit"))) );
    ( {|update_food_amount for sausage, food_amount should be 2|} >:: fun _ ->
      assert_equal 2
        (food_amount (update_food_amount (get_good_food cat "sausage"))) );
  ]

(* BELOW CODE COPIED FROM EH538 A2 submission*)
let parse_test (name : string) (input_str : string) (expected_output : command)
    : test =
  name >:: fun _ -> assert_equal expected_output (parse input_str)

let parse_malformed_test (name : string) (input_str : string) : test =
  name >:: fun _ -> assert_raises Malformed (fun x -> parse input_str)

let parse_empty_test (name : string) (input_str : string) : test =
  name >:: fun _ -> assert_raises Empty (fun x -> parse input_str)

let command_tests =
  [
    parse_test "quit" "quit" Quit;
    parse_test "feed orange juice" "feed orange juice"
      (Feed [ "orange"; "juice" ]);
    parse_test "feed orange juice with excess whitespace"
      "    feed  orange   juice   "
      (Feed [ "orange"; "juice" ]);
    parse_test "feed watermelon" " feed    watermelon    "
      (Feed [ "watermelon" ]);
    parse_malformed_test "feed, with nothing after" "  feed    ";
    parse_malformed_test "quit, with something after" "quit sus";
    parse_empty_test "empty" "";
  ]

(* END CODE COPIED FROM EH538 A2 SUBMISSION*)
let suite =
  "test suite for final project" >::: List.flatten [ pet_tests; command_tests ]

let _ = run_test_tt_main suite
