open OUnit2
open Game
open Pet
open Command

(********************************************************************
   Here are some helper functions for your testing of set-like lists.
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether they are
    equivalent set-like lists. That means checking two things. First, they must
    both be "set-like", meaning that they do not contain any duplicates. Second,
    they must contain the same elements, though not necessarily in the same
    order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(* These tests demonstrate how to use [cmp_set_like_lists] and [pp_list] to get
   helpful output from OUnit. *)
let cmp_demo =
  [
    ( "order is irrelevant" >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        [ "foo"; "bar" ] [ "bar"; "foo" ] )
    (* Uncomment this test to see what happens when a test case fails.
       "duplicates not allowed" >:: (fun _ -> assert_equal
       ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string) ["foo"; "foo"]
       ["foo"]); *);
  ]

(********************************************************************
       End helper functions.
 ********************************************************************)

(*Loading files from data directory*)
let data_dir_prefix = "data" ^ Filename.dir_sep
let sample = Yojson.Basic.from_file (data_dir_prefix ^ "samplejson.json")
let cat = get_pet (pets_of_json sample) "cat"

let pet_tests =
  [
    ( {|name of cat in samplejson.json, should be "cat"|} >:: fun _ ->
      assert_equal "cat" (get_name cat) );
    ( {|health of cat in samplejson.json, should be 100|} >:: fun _ ->
      assert_equal 100 (get_health cat) );
    ( {|gender of cat in samplejson.json, should be male|} >:: fun _ ->
      assert_equal "male" (get_gender cat) );
    ( {|hunger of cat in samplejson.json, should be 10|} >:: fun _ ->
      assert_equal 10 (get_hunger cat) );
    ( {|description of samplejson.json, should be "cat says: 'nice to meet you' in cat language. It looks hungry!"|}
    >:: fun _ ->
      assert_equal
        "cat says: 'nice to meet you' in cat language. It looks hungry!"
        (get_description cat) );
    ( {|a bad food of cat in samplejson.json with input "chocolate", should be "chocolate"|}
    >:: fun _ ->
      assert_equal "chocolate"
        (get_bad_food_name (get_bad_food cat "chocolate")) );
    ( {|a bad food of cat in samplejson.json with input "grapes", should be "grapes"|}
    >:: fun _ ->
      assert_equal "grapes" (get_bad_food_name (get_bad_food cat "grapes")) );
    ( {|a bad food of cat in samplejson.json with input "egg", should be "egg"|}
    >:: fun _ -> assert_equal "egg" (get_bad_food_name (get_bad_food cat "egg"))
    );
    ( {|cat's chocolate effect in samplejson.json, should be -10|} >:: fun _ ->
      assert_equal (-10) (get_bad_food_effect (get_bad_food cat "chocolate")) );
    ( {|cat's grapes effect in samplejson.json, should be -8|} >:: fun _ ->
      assert_equal (-8) (get_bad_food_effect (get_bad_food cat "grapes")) );
    ( {|cat's egg effect in samplejson.json, should be -5|} >:: fun _ ->
      assert_equal (-5) (get_bad_food_effect (get_bad_food cat "egg")) );
    ( {|a good food of cat in samplejson.json with input "cod", should be "cod"|}
    >:: fun _ ->
      assert_equal "cod" (get_good_food_name (get_good_food cat "cod")) );
    ( {|a good food of cat in samplejson.json with input "milk", should be "milk"|}
    >:: fun _ ->
      assert_equal "milk" (get_good_food_name (get_good_food cat "milk")) );
    ( {|a good food of cat in samplejson.json with input "biscuit", should be "biscuit"|}
    >:: fun _ ->
      assert_equal "biscuit" (get_good_food_name (get_good_food cat "biscuit"))
    );
    ( {|a good food of cat in samplejson.json with input "sausage", should be "sausage"|}
    >:: fun _ ->
      assert_equal "sausage" (get_good_food_name (get_good_food cat "sausage"))
    );
    ( {|cat's cod effect in samplejson.json, should be 5|} >:: fun _ ->
      assert_equal 5 (get_good_food_effect (get_good_food cat "cod")) );
    ( {|cat's milk effect in samplejson.json, should be 3|} >:: fun _ ->
      assert_equal 3 (get_good_food_effect (get_good_food cat "milk")) );
    ( {|cat's biscuit effect in samplejson.json, should be 1|} >:: fun _ ->
      assert_equal 1 (get_good_food_effect (get_good_food cat "biscuit")) );
    ( {|cat's sausage effect in samplejson.json, should be 2|} >:: fun _ ->
      assert_equal 2 (get_good_food_effect (get_good_food cat "sausage")) );
    ( {|cat's health after being fed chocolate, should be 90|} >:: fun _ ->
      assert_equal 90
        (get_health
           (update_pet_health cat
              (get_bad_food_effect (get_bad_food cat "chocolate")))) );
    ( {|cat's health after being fed grapes, should be 92|} >:: fun _ ->
      assert_equal 92
        (get_health
           (update_pet_health cat
              (get_bad_food_effect (get_bad_food cat "grapes")))) );
    ( {|cat's health after being fed egg, should be 95|} >:: fun _ ->
      assert_equal 95
        (get_health
           (update_pet_health cat
              (get_bad_food_effect (get_bad_food cat "egg")))) );
    ( {|cat's health after being fed cod, should be 105|} >:: fun _ ->
      assert_equal 105
        (get_health
           (update_pet_health cat
              (get_good_food_effect (get_good_food cat "cod")))) );
    ( {|cat's health after being fed milk, should be 103|} >:: fun _ ->
      assert_equal 103
        (get_health
           (update_pet_health cat
              (get_good_food_effect (get_good_food cat "milk")))) );
    ( {|cat's health after being fed biscuit, should be 101|} >:: fun _ ->
      assert_equal 101
        (get_health
           (update_pet_health cat
              (get_good_food_effect (get_good_food cat "biscuit")))) );
    ( {|cat's health after being fed sausage, should be 102|} >:: fun _ ->
      assert_equal 102
        (get_health
           (update_pet_health cat
              (get_good_food_effect (get_good_food cat "sausage")))) );
    ( {|cat's hygiene after being bathed, should be 125|} >:: fun _ ->
      assert_equal 125 (get_hygiene (update_pet_hygiene cat 25)) );
    ( {|cat's hygiene after being brushed, should be 110|} >:: fun _ ->
      assert_equal 110 (get_hygiene (update_pet_hygiene cat 10)) );
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
  "test suite for final project"
  >::: List.flatten [ cmp_demo; pet_tests; command_tests ]

let _ = run_test_tt_main suite
