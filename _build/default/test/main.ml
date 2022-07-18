open OUnit2
open Risk
open Territories
open Cards
open Player
open Draft
open Battle
open Fortify
open Init

(* Testing Plan: To test and ensure the correctness of our project we
   used a combination of OUnit and manual tests. In general, for any
   function that was based on random chance or produced different
   outputs every time we ran it, we used manual tests. And for any
   function that was designed to produce the same output for every
   input, we tested with OUnit. We also choose not to include helper
   functions in our OUnit tests, because we tested other functions that
   were dependent on the correctness of the helpers.

   OUnit: Our OUnit test suite composed of tests from 6/7 of our central
   modules: Territories, Cards, Player, Draft, Battle, and Fortify. We
   chose to omit the Init module from our test suite because this module
   handled the setup of the game, which is partially based on random
   chance. Each time a game is run with the Init module a randomized set
   of territories and troops is distributed to each player, so we tested
   the game initialization code with manual testing. For the remaining
   modules we tested all of the functions in them, minus helper
   functions or functions that were based on chance or randomization.

   Manually: We chose to manually test function like [fully_init_game],
   [odds_main], and [battle simulator] because they produced varying
   outputs every time they ran. For example, to test [odds_main], which
   shows probability of player winning attack based off simulations, we
   compared the official Risk combat statistics to the general numbers
   we were getting from out simulations. If we got 84.76% or 85.41% odds
   of winning an attack and the official statistics read 85%, we
   consider that case to be correct. For other functions that also
   relied on randomization, we used a similar testing methodology.

   The test cases were developed for this project with a combination of
   black-box and glass-box testing strategies. Black-box tests were
   developed by following the specifications for functions from the MLI.
   Specifically, the inputs we used for the test cases involved typical
   inputs, paths through specification, and boundary cases. Black-box
   testing was a good start for the test cases, but wasn’t enough, so we
   also incorporated glass-box testing. With glass-box testing we
   checked that every path for functions was. To do this we incorporated
   different inputs and ensured that they reached every path in the
   functions.

   Our testing approach proves the correctness of the system because we
   used a thorough approach involving black-box and glass-box testing.
   Each path of the functions was covered and a variety of inputs were
   tested given the specifications in the MLI (typical, boundary,
   paths). In addition, the functions that were based on randomness were
   manually tested. We also tested our project by simply playing the
   game. In the end our game is fully playable and functional. Overall,
   our project was thoroughly tested and proven correct. *)

(*Territory functions*)
let is_border_test
    (name : string)
    (territory_1 : territory)
    (territory_2 : territory)
    (expected : bool) =
  name >:: fun _ ->
  assert_equal expected (is_border territory_1 territory_2)

let is_territory_test
    (name : string)
    (territory : string)
    (expected : bool) =
  name >:: fun _ -> assert_equal expected (is_territory territory)

let to_territory_test
    (name : string)
    (territory : string)
    (expected : territory) =
  name >:: fun _ ->
  assert_equal expected (to_territory territory all_territories)

let say_borders_test
    (name : string)
    (place : territory)
    (expected : string list) =
  name >:: fun _ -> assert_equal expected (say_borders place)

let sub_list_test
    (name : string)
    (continent_iq : territory list)
    (owned : territory list)
    (expected : bool) =
  name >:: fun _ -> assert_equal expected (sub_list continent_iq owned)

(*Cards functions*)
let card_count_test
    (name : string)
    (cards : card list)
    (expected : int list) =
  name >:: fun _ -> assert_equal expected (card_count cards)

let has_pattern_test
    (name : string)
    (cards : card list)
    (expected : bool) =
  name >:: fun _ -> assert_equal expected (has_pattern cards)

let pattern_troops_test
    (name : string)
    (cards : card list)
    (expected : int) =
  name >:: fun _ -> assert_equal expected (pattern_troops cards)

let must_trade_in_test
    (name : string)
    (cards : card list)
    (expected : bool) =
  name >:: fun _ -> assert_equal expected (must_trade_in cards)

let remove_cards_test
    (name : string)
    (cards : card list)
    (bonus : int)
    (expected : card list) =
  name >:: fun _ -> assert_equal expected (remove_cards cards bonus)

(*Player function*)
let to_owned_terr_test
    (name : string)
    (owned : owned_territory list)
    (terr : territory)
    (expected : owned_territory) =
  name >:: fun _ -> assert_equal expected (to_owned_terr owned terr)

let terrs_to_string_test
    (name : string)
    (terrs : owned_territory list)
    (expected : string) =
  name >:: fun _ -> assert_equal expected (terrs_to_string terrs)

let name_list_owned_test
    (name : string)
    (terrs : owned_territory list)
    (expected : string list) =
  name >:: fun _ -> assert_equal expected (name_list_owned terrs)

let num_troops_in_terr_test
    (name : string)
    (terr : string)
    (player : player)
    (expected : int) =
  name >:: fun _ ->
  assert_equal expected (num_troops_in_terr terr player)

let list_of_available_borders_test
    (name : string)
    (borders : string list)
    (player : player)
    (expected : string list) =
  name >:: fun _ ->
  assert_equal expected (list_of_available_borders borders player)

let winner_test
    (name : string)
    (p : player)
    (stage : territory list)
    (expected : bool) =
  name >:: fun _ -> assert_equal expected (winner p stage)

let lost_test (name : string) (p : player) (expected : bool) =
  name >:: fun _ -> assert_equal expected (lost p)

let show_cards_test (name : string) (p : player) (expected : string) =
  name >:: fun _ -> assert_equal expected (show_cards p)

(*Draft functions*)
let count_terrs_test
    (name : string)
    (terrs : owned_territory list)
    (expected : int) =
  name >:: fun _ -> assert_equal expected (count_terrs terrs)

let continent_bonus_test
    (name : string)
    (continents : continent list)
    (p : player)
    (expected : int) =
  name >:: fun _ -> assert_equal expected (continent_bonus continents p)

let allocate_troops_test (name : string) (p : player) (expected : int) =
  name >:: fun _ -> assert_equal expected (allocate_troops p)

let add_troops_test
    (name : string)
    (p : player)
    (terr : territory)
    (num : int)
    (expected : player) =
  name >:: fun _ -> assert_equal expected (add_troops p terr num)

(*Battle function*)
let battle_simulator_test
    (name : string)
    (terr_1 : owned_territory)
    (terr_2 : owned_territory)
    (expected : owned_territory) =
  name >:: fun _ ->
  assert_equal expected (battle_simulator terr_1 terr_2)

let odds_main_test
    (name : string)
    (terr_1 : owned_territory)
    (terr_2 : owned_territory)
    (iterations : int)
    (expected : float) =
  name >:: fun _ ->
  assert_equal expected (odds_main terr_1 terr_2 iterations)

(*Fortify functions*)
let to_terr_test
    (name : string)
    (terr_list : owned_territory list)
    (expected : territory list) =
  name >:: fun _ -> assert_equal expected (to_terr terr_list)

let can_fortify_test
    (name : string)
    (curr_player : player)
    (terr : territory)
    (expected : territory list) =
  name >:: fun _ ->
  assert_equal expected (can_fortify curr_player terr all_territories)

let fortify_test
    (name : string)
    (curr_player : player)
    (troop : int)
    (terr1 : territory)
    (terr2 : territory)
    (expected : player) =
  name >:: fun _ ->
  assert_equal expected
    (fortify curr_player troop terr1 terr2 all_territories)

(*Init functions*)
let fully_init_game_test
    (name : string)
    (p1 : string)
    (p2 : string)
    (p3 : string)
    (p4 : string)
    (expected : gamestate) =
  name >:: fun _ ->
  assert_equal expected (fully_init_game p1 p2 p3 p4 0 all_territories)

(*Create list with test cases for each module*)
let territories_tests =
  [
    (* is_border Tests *)
    is_border_test "Testing [is_border] with Peru's borders" peru japan
      false;
    is_border_test "Testing [is_border] with Peru's borders 2" peru
      brazil true;
    is_border_test "Testing [is_border] with Japan's borders" japan
      mongolia true;
    is_border_test "Testing [is_border] with Japan's borders 2" japan
      siam false;
    (* to_territory Tests *)
    is_territory_test "Testing [is_territory] on Japan" "Japan" true;
    is_territory_test "Testing [is_territory] on North Korea"
      "North Korea" false;
    (* says_borders Tests *)
    say_borders_test "Testing [says_borders] on Iceland's borders"
      iceland
      [ "greenland"; "great britain"; "scandinavia" ];
    say_borders_test "Testing [says_borders] on Iceland's borders"
      northern_europe
      [
        "great britain";
        "scandinavia";
        "great britain";
        "western europe";
        "southern europe";
        "ukraine";
      ]
    (* sub_list Tests *);
  ]

let cards_tests =
  [
    (* card_count Tests *)
    card_count_test "Testing [card_count] on empty list" [] [ 0; 0; 0 ];
    card_count_test "Testing [card_count] on singular card list"
      [ Troop; Horse; Cannon ]
      [ 1; 1; 1 ];
    card_count_test "Testing [card_count] on singular card list"
      [ Cannon; Horse; Troop; Troop; Troop; Horse; Horse; Cannon ]
      [ 3; 3; 2 ];
    (* has_pattern Tests *)
    has_pattern_test "Testing [has_pattern] on empty list" [] false;
    has_pattern_test "Testing [has_pattern] on cards w/ no pattern"
      [ Cannon; Horse ] false;
    has_pattern_test "Testing [has_pattern] on cards w/ pattern"
      [ Cannon; Cannon; Cannon ]
      true;
    (* pattern_troops Tests *)
    pattern_troops_test "Testing [pattern_troops] on empty list" [] 0;
    pattern_troops_test
      "Testing [pattern_troops] on cards w/ no pattern" [] 0;
    pattern_troops_test "Testing [pattern_troops] on empty w/ pattern"
      [ Troop; Troop; Troop ] 4;
    pattern_troops_test "Testing [pattern_troops] on empty w/ pattern"
      [ Troop; Horse; Cannon ]
      10;
    (* must_trade_in Tests *)
    must_trade_in_test "Testing [must_trade_in] on small card list"
      [ Troop ] false;
    must_trade_in_test "Testing [must_trade_in] on large card list"
      [ Troop; Cannon; Horse; Troop; Cannon ]
      true
    (* remove_cards Tests *);
    remove_cards_test "Testing [remove_cards] on list w/ every card"
      [ Troop; Cannon; Horse ]
      10 [];
    remove_cards_test
      "Testing [remove_cards] on list w/ many troop cards"
      [ Troop; Troop; Troop; Troop; Troop ]
      4 [ Troop; Troop ];
    remove_cards_test
      "Testing [remove_cards] on large list w/ many horses, one cannon"
      [ Horse; Horse; Horse; Cannon ]
      6 [ Cannon ];
  ]

let player_tests =
  [
    (* to_owned_terr Tests *)
    to_owned_terr_test
      "Testing [to_owned_terr] on singular list of owned territories"
      [ { place = peru; troops = 5 } ]
      peru
      { place = peru; troops = 5 };
    to_owned_terr_test
      "Testing [to_owned_terr] on small list of owned territories"
      [
        { place = peru; troops = 5 };
        { place = alaska; troops = 17 };
        { place = western_us; troops = 7 };
      ]
      alaska
      { place = alaska; troops = 17 };
    (* terrs_to_string Tests *)
    terrs_to_string_test
      "Testing [terrs_to_string] on singular list of owned territories"
      [ { place = peru; troops = 5 } ]
      "peru : 5\n";
    terrs_to_string_test
      "Testing [terrs_to_string] on small list of owned territories"
      [
        { place = peru; troops = 5 };
        { place = alaska; troops = 17 };
        { place = western_us; troops = 7 };
      ]
      "peru : 5\nalaska : 17\nwestern us : 7\n";
    (* name_list_owned Tests *)
    name_list_owned_test
      "Testing [name_list_owned] on singular list of owned territories"
      [ { place = peru; troops = 5 } ]
      [ "peru" ];
    name_list_owned_test
      "Testing [name_list_owned] on small list of owned territories"
      [
        { place = peru; troops = 5 };
        { place = alaska; troops = 17 };
        { place = western_us; troops = 7 };
      ]
      [ "peru"; "alaska"; "western us" ];
    (* num_troops_in_terr Tests *)
    num_troops_in_terr_test
      "Testing [num_troops_in_terr] on player /w several territories"
      "peru"
      {
        name = "Jackson";
        terrs =
          [
            { place = peru; troops = 5 };
            { place = alaska; troops = 17 };
            { place = western_us; troops = 7 };
          ];
        cards = [];
      }
      5;
    (* list_of_available_borders Tests *)
    list_of_available_borders_test
      "Testing [list_of_available_borders] on player /w several \
       territories"
      [ "brazil" ]
      {
        name = "Jackson";
        terrs = [ { place = peru; troops = 5 } ];
        cards = [];
      }
      [ "brazil" ];
    (* winner Tests *)
    winner_test
      "Testing [winner] on player who does not have all territories on \
       stage"
      {
        name = "Jackson";
        terrs = [ { place = peru; troops = 5 } ];
        cards = [];
      }
      [ peru; alaska ] false;
    winner_test
      "Testing [winner] on player who does have all territories on \
       stage"
      {
        name = "Jackson";
        terrs =
          [
            { place = peru; troops = 5 }; { place = alaska; troops = 5 };
          ];
        cards = [];
      }
      [ peru; alaska ] true;
    (* lost Tests *)
    lost_test "Testing [lost] on player w/ no territories"
      { name = "Jackson"; terrs = []; cards = [] }
      true;
    lost_test "Testing [lost] on player w/ territories"
      {
        name = "Jackson";
        terrs =
          [
            { place = peru; troops = 5 }; { place = alaska; troops = 5 };
          ];
        cards = [];
      }
      false;
    (* show_cads Tests *)
    show_cards_test "Testing show_cards on player w/ no cards"
      {
        name = "Jackson";
        terrs =
          [
            { place = peru; troops = 5 }; { place = alaska; troops = 5 };
          ];
        cards = [];
      }
      "Card \n";
    show_cards_test "Testing [show_cards] on player w/ several cards"
      {
        name = "Jackson";
        terrs =
          [
            { place = peru; troops = 5 }; { place = alaska; troops = 5 };
          ];
        cards = [ Cannon; Cannon; Cannon ];
      }
      "Card \n\n1. Cannon\n2. Cannon\n3. Cannon";
  ]

let draft_tests =
  [
    count_terrs_test "Testing [count_terrs] on empty territory list" []
      0;
    count_terrs_test "Testing [count_terrs] on non-empty territory list"
      [
        { place = peru; troops = 5 };
        { place = alaska; troops = 5 };
        { place = western_us; troops = 5 };
      ]
      3;
    continent_bonus_test
      "Testing [continent_bonus] on empty continent list" []
      {
        name = "Jackson";
        terrs = [ { place = peru; troops = 5 } ];
        cards = [];
      }
      0;
    continent_bonus_test
      "Testing [continent_bonus] on continent list /w Europe w/ player \
       that does not have all needed territories"
      [ europe ]
      {
        name = "Jackson";
        terrs = [ { place = peru; troops = 5 } ];
        cards = [];
      }
      0;
    continent_bonus_test
      "Testing [continent_bonus] on continent list /w Europe" [ europe ]
      {
        name = "Jackson";
        terrs =
          [
            { place = iceland; troops = 5 };
            { place = great_britain; troops = 5 };
            { place = western_europe; troops = 5 };
            { place = southern_europe; troops = 5 };
            { place = northern_europe; troops = 5 };
            { place = scandinavia; troops = 5 };
            { place = ukraine; troops = 5 };
          ];
        cards = [];
      }
      5;
    allocate_troops_test "Testing [allocate_troops] on player"
      {
        name = "Jackson";
        terrs =
          [
            { place = iceland; troops = 1 };
            { place = great_britain; troops = 1 };
            { place = western_europe; troops = 1 };
            { place = southern_europe; troops = 1 };
            { place = northern_europe; troops = 1 };
            { place = scandinavia; troops = 1 };
            { place = ukraine; troops = 1 };
          ];
        cards = [];
      }
      3;
    allocate_troops_test
      "Testing [allocate_troops] on player /w no territories"
      { name = "Jackson"; terrs = []; cards = [] }
      3;
    add_troops_test
      "Testing [add_troops] on player adding 10 troops to iceland"
      {
        name = "Jackson";
        terrs =
          [
            { place = iceland; troops = 1 };
            { place = great_britain; troops = 1 };
            { place = western_europe; troops = 1 };
            { place = southern_europe; troops = 1 };
            { place = northern_europe; troops = 1 };
            { place = scandinavia; troops = 1 };
            { place = ukraine; troops = 1 };
          ];
        cards = [];
      }
      iceland 10
      {
        name = "Jackson";
        terrs =
          [
            { place = iceland; troops = 11 };
            { place = great_britain; troops = 1 };
            { place = western_europe; troops = 1 };
            { place = southern_europe; troops = 1 };
            { place = northern_europe; troops = 1 };
            { place = scandinavia; troops = 1 };
            { place = ukraine; troops = 1 };
          ];
        cards = [];
      };
  ]

let battle_tests =
  [
    battle_simulator_test
      "Testing [battle simulator] on two territories where one is \
       garunteed win"
      { place = peru; troops = 0 }
      { place = western_us; troops = 100 }
      { place = western_us; troops = 100 };
    odds_main_test "Testing [odds_main] on terrs with 100 and 0 troops"
      { place = peru; troops = 0 }
      { place = western_us; troops = 100 }
      1 0.0;
    odds_main_test "Testing [odds_main] on terrs with 0 and 100 troops"
      { place = peru; troops = 100 }
      { place = western_us; troops = 0 }
      1 1.0;
  ]

let fortify_tests =
  [
    to_terr_test "Testing [to_terr] on empty owned_terr list" [] [];
    to_terr_test "Testing [to_terr] on singular owned_terr list"
      [ { place = peru; troops = 5 } ]
      [ peru ];
    to_terr_test "Testing [to_terr] on small owned_terr list"
      [
        { place = peru; troops = 5 };
        { place = western_australia; troops = 5 };
        { place = western_europe; troops = 5 };
        { place = western_us; troops = 5 };
      ]
      [ peru; western_australia; western_europe; western_us ];
    can_fortify_test "Testing [can_fortify] on player w/ 1 territory"
      {
        name = "Jackson";
        terrs = [ { place = iceland; troops = 5 } ];
        cards = [];
      }
      iceland [];
    can_fortify_test
      "Testing [can_fortify] on player with several connecting \
       territories"
      {
        name = "Jackson";
        terrs =
          [
            { place = iceland; troops = 5 };
            { place = greenland; troops = 5 };
            { place = great_britain; troops = 5 };
          ];
        cards = [];
      }
      iceland
      [ great_britain; greenland ];
    can_fortify_test
      "Testing [can_fortify] on player with several connecting \
       territories"
      {
        name = "Jackson";
        terrs =
          [
            { place = alberta; troops = 5 };
            { place = alaska; troops = 5 };
            { place = ontario; troops = 5 };
          ];
        cards = [];
      }
      alberta [ ontario; alaska ];
    fortify_test "Testing [fortify] on player w/ several territories"
      {
        name = "Jackson";
        terrs =
          [
            { place = alberta; troops = 5 };
            { place = alaska; troops = 5 };
            { place = ontario; troops = 5 };
          ];
        cards = [];
      }
      3 alberta alaska
      {
        name = "Jackson";
        terrs =
          [
            { place = ontario; troops = 5 };
            { place = alberta; troops = 2 };
            { place = alaska; troops = 8 };
          ];
        cards = [];
      };
  ]

let suite =
  "test suite for Risk"
  >::: List.flatten
         [
           territories_tests;
           cards_tests;
           player_tests;
           draft_tests;
           battle_tests;
           fortify_tests;
         ]

let _ = run_test_tt_main suite