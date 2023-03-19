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
    let indiv_pet_sample = Yojson.Basic.from_file (data_dir_prefix ^ "individual_pet.json")
     (*Pet tests*)

    let cat =  get_pet(pets_of_json sample) "cat"
     let pet_tests = [( {|name of samplejson.json, should be "cat"|} >:: fun _ ->
      assert_equal "cat" (getName (cat)) );
      
      ( {|health of samplejson.json, should be 100|} >:: fun _ ->
        assert_equal 100 (getHealth (cat)) );
        ( {|hunger of samplejson.json, should be 0|} >:: fun _ ->
          assert_equal 0 (getHunger (cat)) );
          ( {|description of samplejson.json, should be "a normal cat"|} >:: fun _ ->
            assert_equal "a normal cat" (getDescription (cat)) );]


let command_tests = []

let suite =
  "test suite for final project"
  >::: List.flatten [ cmp_demo; pet_tests; command_tests; ]

let _ = run_test_tt_main suite