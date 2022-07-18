module Territories = struct
  type territory = {
    name : string;
    borders : string list;
  }

  type continent = {
    parts : territory list;
    bonus : int;
  }

  let alaska =
    {
      name = "alaska";
      borders = [ "northwest territory"; "alberta"; "kamchatka" ];
    }

  let northwest_territory =
    {
      name = "northwest territory";
      borders = [ "alaska"; "alberta"; "ontario"; "greenland" ];
    }

  let alberta =
    {
      name = "alberta";
      borders =
        [ "northwest territory"; "alaska"; "western us"; "ontario" ];
    }

  let western_us =
    {
      name = "western us";
      borders =
        [ "alberta"; "ontario"; "eastern us"; "central america" ];
    }

  let central_america =
    {
      name = "central america";
      borders = [ "western us"; "eastern us"; "venezuela" ];
    }

  let eastern_us =
    {
      name = "eastern us";
      borders = [ "central america"; "western us"; "ontario"; "quebec" ];
    }

  let ontario =
    {
      name = "ontario";
      borders =
        [
          "northwest territory";
          "alberta";
          "western us";
          "eastern us";
          "quebec";
          "greenland";
        ];
    }

  let quebec =
    {
      name = "quebec";
      borders = [ "eastern us"; "ontario"; "greenland" ];
    }

  let greenland =
    {
      name = "greenland";
      borders =
        [ "northwest territory"; "ontario"; "quebec"; "iceland" ];
    }

  let venezuela =
    {
      name = "venezuela";
      borders = [ "central America"; "brazil"; "peru" ];
    }

  let peru =
    { name = "peru"; borders = [ "venezuela"; "brazil"; "argentina" ] }

  let argentina = { name = "argentina"; borders = [ "peru"; "brazil" ] }

  let brazil =
    {
      name = "brazil";
      borders = [ "venezuela"; "peru"; "argentina"; "north africa" ];
    }

  let north_africa =
    {
      name = "north africa";
      borders =
        [
          "brazil";
          "western europe";
          "southern europe";
          "egypt";
          "east africa";
          "congo";
        ];
    }

  let congo =
    {
      name = "congo";
      borders = [ "north africa"; "east africa"; "south africa" ];
    }

  let south_africa =
    {
      name = "south africa";
      borders = [ "madagascar"; "congo"; "east africa" ];
    }

  let egypt =
    {
      name = "egypt";
      borders =
        [
          "north africa";
          "southern europe";
          "middle east";
          "east africa";
        ];
    }

  let madagascar =
    { name = "madagascar"; borders = [ "south africa"; "east africa" ] }

  let east_africa =
    {
      name = "east africa";
      borders = [ "egypt"; "congo"; "south africa"; "madagascar" ];
    }

  let western_europe =
    {
      name = "western europe";
      borders =
        [
          "great britain";
          "north africa";
          "northern europe";
          "southern europe";
        ];
    }

  let great_britain =
    {
      name = "great britain";
      borders =
        [
          "iceland"; "scandinavia"; "northern europe"; "western europe";
        ];
    }

  let iceland =
    {
      name = "iceland";
      borders = [ "greenland"; "great britain"; "scandinavia" ];
    }

  let scandinavia =
    {
      name = "scandinavia";
      borders =
        [ "iceland"; "great britain"; "northern europe"; "ukraine" ];
    }

  let northern_europe =
    {
      name = "northern europe";
      borders =
        [
          "great britain";
          "scandinavia";
          "great britain";
          "western europe";
          "southern europe";
          "ukraine";
        ];
    }

  let southern_europe =
    {
      name = "southern europe";
      borders =
        [
          "north africa";
          "egypt";
          "western europe";
          "northern europe";
          "ukraine";
          "middle east";
        ];
    }

  let ukraine =
    {
      name = "ukraine";
      borders =
        [
          "scandinavia";
          "northern europe";
          "southern europe";
          "middle east";
          "afghanistan";
          "ural";
        ];
    }

  let indonesia =
    {
      name = "indonesia";
      borders =
        [
          "siam"; "new guinea"; "western australia"; "eastern australia";
        ];
    }

  let new_guinea =
    {
      name = "new guinea";
      borders =
        [ "indonesia"; "western australia"; "eastern australia" ];
    }

  let western_australia =
    {
      name = "western australia";
      borders = [ "eastern australia"; "new guinea"; "indonesia" ];
    }

  let eastern_australia =
    {
      name = "eastern australia";
      borders = [ "western australia"; "new Guinea" ];
    }

  let middle_east =
    {
      name = "middle east";
      borders =
        [
          "egypt"; "southern europe"; "ukraine"; "afghanistan"; "india";
        ];
    }

  let afghanistan =
    {
      name = "afghanistan";
      borders = [ "ukraine"; "middle east"; "ural"; "china"; "india" ];
    }

  let china =
    {
      name = "china";
      borders =
        [
          "siam"; "india"; "afghanistan"; "ural"; "siberia"; "mongolia";
        ];
    }

  let india =
    {
      name = "india";
      borders = [ "middle east"; "afghanistan"; "china"; "siam" ];
    }

  let irkutsk =
    {
      name = "irkutsk";
      borders = [ "mogolia"; "siberia"; "yakutsk"; "kamchatka" ];
    }

  let japan = { name = "japan"; borders = [ "mongolia"; "kamchatka" ] }

  let kamchatka =
    {
      name = "kamchatka";
      borders = [ "yakutsk"; "irkutsk"; "mongolia"; "japan"; "alaska" ];
    }

  let mongolia =
    {
      name = "mongolia";
      borders = [ "china"; "siberia"; "irkutsk"; "kamchatka"; "japan" ];
    }

  let siam =
    { name = "siam"; borders = [ "india"; "china"; "indonesia" ] }

  let siberia =
    {
      name = "siberia";
      borders = [ "ural"; "china"; "yakutsk"; "irkutsk"; "mongolia" ];
    }

  let ural =
    {
      name = "ural";
      borders = [ "ukraine"; "afghanistan"; "siberia"; "china" ];
    }

  let yakutsk =
    {
      name = "yakutsk";
      borders = [ "siberia"; "irkutsk"; "kamchatka" ];
    }

  let asia : continent =
    {
      parts =
        [
          middle_east;
          afghanistan;
          china;
          india;
          irkutsk;
          japan;
          kamchatka;
          mongolia;
          siam;
          siberia;
          ural;
          yakutsk;
        ];
      bonus = 7;
    }

  let australia : continent =
    {
      parts =
        [ indonesia; new_guinea; western_australia; eastern_australia ];
      bonus = 2;
    }

  let europe : continent =
    {
      parts =
        [
          iceland;
          great_britain;
          western_europe;
          southern_europe;
          northern_europe;
          scandinavia;
          ukraine;
        ];
      bonus = 5;
    }

  let africa : continent =
    {
      parts =
        [
          north_africa;
          congo;
          south_africa;
          madagascar;
          east_africa;
          egypt;
        ];
      bonus = 3;
    }

  let south_america : continent =
    { parts = [ venezuela; peru; argentina; brazil ]; bonus = 2 }

  let north_america : continent =
    {
      parts =
        [
          alaska;
          northwest_territory;
          alberta;
          western_us;
          central_america;
          eastern_us;
          ontario;
          quebec;
          greenland;
        ];
      bonus = 5;
    }

  let all_territories : territory list =
    north_america.parts @ south_america.parts @ africa.parts
    @ europe.parts @ australia.parts @ asia.parts

  let all_continents : continent list =
    [ asia; australia; europe; africa; south_america; north_america ]

  (*A1 A2*)
  (*B1 B2*)
  let a_1 = { name = "a1"; borders = [ "a2"; "b1" ] }
  let a_2 = { name = "a2"; borders = [ "a1"; "b2" ] }
  let b_1 = { name = "b1"; borders = [ "b2"; "a1" ] }
  let b_2 = { name = "b2"; borders = [ "b1"; "a2" ] }
  let continentsfake = [ a_1; a_2; b_1; b_2 ]

  let is_border (territory_1 : territory) (territory_2 : territory) :
      bool =
    let comparef x = if x = territory_1.name then true else false in
    List.exists comparef territory_2.borders

  let rec is_territory_helper (name : string) (list : territory list) :
      bool =
    match list with
    | [] -> false
    | head :: t ->
        if head.name = name then true else is_territory_helper name t

  let is_territory (name : string) : bool =
    is_territory_helper (String.lowercase_ascii name) all_territories

  exception InvalidTerritory

  let rec to_territory_helper (name : string) (stage : territory list) :
      territory =
    match stage with
    | [] -> raise InvalidTerritory
    | head :: t ->
        if head.name = name then head else to_territory_helper name t

  let to_territory (name : string) list : territory =
    to_territory_helper (String.lowercase_ascii name) list

  (**[say_borders] lists the borders of [place]*)
  let say_borders (place : territory) = place.borders

  (** [get_continent_territories] reveals continent territories*)
  let get_continent_territories (a : continent) = a.parts

  (**[sub_list] says whether player owns a given [continent_iq]*)
  let rec sub_list
      (continent_iq : territory list)
      (owned : territory list) =
    match continent_iq with
    | [] -> true
    | h :: t ->
        let comparef x = if x = h then true else false in
        if List.exists comparef owned then sub_list t owned else false

  let name_list_terrs (owned : territory list) : string list =
    List.map (fun x -> x.name) owned
end

module Cards = struct
  type card =
    | Troop
    | Horse
    | Cannon

  let rec count_troop (cards : card list) : int =
    match cards with
    | [] -> 0
    | Troop :: t -> 1 + count_troop t
    | _ :: t -> count_troop t

  let rec count_horse (cards : card list) : int =
    match cards with
    | [] -> 0
    | Horse :: t -> 1 + count_horse t
    | _ :: t -> count_horse t

  let rec count_cannon (cards : card list) : int =
    match cards with
    | [] -> 0
    | Cannon :: t -> 1 + count_cannon t
    | _ :: t -> count_cannon t

  let card_count (cards : card list) : int list =
    [ count_troop cards; count_horse cards; count_cannon cards ]

  let has_pattern (cards : card list) : bool =
    let ccount = card_count cards in
    match ccount with
    | troop :: horse :: cannon :: _ ->
        if troop > 0 && horse > 0 && cannon > 0 then true
        else if troop >= 3 || horse >= 3 || cannon >= 3 then true
        else false
    | _ -> false

  let pattern_troops (cards : card list) : int =
    let ccount = card_count cards in
    match ccount with
    | troop :: horse :: cannon :: _ ->
        if troop > 0 && horse > 0 && cannon > 0 then 10
        else if cannon >= 3 then 8
        else if horse >= 3 then 6
        else if troop >= 3 then 4
        else 0
    | _ -> 0

  let must_trade_in (cards : card list) = List.length cards > 4

  let rec add_back (cards : card list) (card : card) (num : int) =
    if num = 0 then cards else add_back (card :: cards) card (num - 1)

  let remove_cards_help bonus cards ccount =
    if bonus = 6 then
      add_back
        (List.filter
           (fun (x : card) ->
             match x with
             | Horse -> false
             | _ -> true)
           cards)
        Horse
        (List.nth ccount 1 - 3)
    else
      add_back
        (List.filter
           (fun (x : card) ->
             match x with
             | Troop -> false
             | _ -> true)
           cards)
        Troop
        (List.nth ccount 0 - 3)

  let remove_cards (cards : card list) (bonus : int) =
    let ccount = card_count cards in
    if bonus = 10 then
      let new_cards = [] in
      let with_troops =
        add_back new_cards Troop (List.nth ccount 0 - 1)
      in
      let with_horses =
        add_back with_troops Horse (List.nth ccount 1 - 1)
      in
      add_back with_horses Cannon (List.nth ccount 2 - 1)
    else if bonus = 8 then
      add_back
        (List.filter
           (fun (x : card) ->
             match x with
             | Cannon -> false
             | _ -> true)
           cards)
        Cannon
        (List.nth ccount 2 - 3)
    else remove_cards_help bonus cards ccount
end

module Player = struct
  open Territories
  open Cards

  type owned_territory = {
    place : territory;
    troops : int;
  }

  type player = {
    name : string;
    terrs : owned_territory list;
    cards : card list;
  }

  let rec to_owned_terr
      (owned : owned_territory list)
      (terr : territory) : owned_territory =
    match owned with
    | [] -> raise Not_found
    | h :: t -> if h.place = terr then h else to_owned_terr t terr

  let rec terrs_to_string terrs =
    match terrs with
    | [] -> ""
    | h :: t ->
        h.place.name ^ " : " ^ string_of_int h.troops ^ "\n"
        ^ terrs_to_string t

  let name_list_owned (owned : owned_territory list) : string list =
    List.map (fun x -> x.place.name) owned

  let rec num_troops_in_terr (terr : string) (player : player) =
    match player.terrs with
    | [] -> 0
    | h :: t ->
        if h.place.name = terr then h.troops
        else num_troops_in_terr terr { player with terrs = t }

  let rec list_of_available_borders borders player =
    match borders with
    | [] -> []
    | h :: t ->
        if List.mem h (name_list_owned player.terrs) then
          list_of_available_borders t player
        else h :: list_of_available_borders t player

  let rec owned_to_terr_list (list : owned_territory list) :
      territory list =
    match list with
    | [] -> []
    | h :: t -> h.place :: owned_to_terr_list t

  let rec winner_helper
      (plist : territory list)
      (tlist : territory list) : bool =
    match tlist with
    | [] -> true
    | h :: t ->
        if List.mem h plist then winner_helper plist t else false

  let winner (p : player) stage : bool =
    let plist = owned_to_terr_list p.terrs in
    winner_helper plist stage

  let lost (p : player) : bool =
    match p.terrs with
    | [] -> true
    | list -> false

  let card_to_string (c : card) : string =
    if c = Troop then "Troop"
    else if c = Cannon then "Cannon"
    else "Horse"

  let rec show_cards_helper (number : int) (cards : card list) : string
      =
    match cards with
    | [] -> ""
    | h :: t ->
        "\n"
        ^ string_of_int (number + 1)
        ^ ". " ^ card_to_string h
        ^ show_cards_helper (number + 1) t

  let show_cards (p : player) : string =
    "Card \n" ^ show_cards_helper 0 p.cards

  let give_card (p : player) : player =
    let x = int_of_float (Unix.time ()) mod 3 in
    let card =
      if x = 0 then Troop else if x = 1 then Horse else Cannon
    in
    { p with cards = card :: p.cards }
end

module Draft = struct
  open Player
  open Territories
  open Cards

  (* [count_troops] calculates number of territories that a player
     owns*)
  let rec count_terrs (terrs : owned_territory list) : int =
    match terrs with
    | [] -> 0
    | _ :: t -> 1 + count_terrs t

  let rec check_player_owns_continent
      (continent_terrs : territory list)
      (player_terrs : territory list) =
    match continent_terrs with
    | [] -> true
    | h :: t ->
        if List.mem h player_terrs then
          check_player_owns_continent t player_terrs
        else false

  (* [continent_bonus] outputs a player's bonus for holding continents*)
  let rec continent_bonus (continents : continent list) (p : player) :
      int =
    match continents with
    | [] -> 0
    | h :: t ->
        if
          (* needs to check whether player has all territories in
             continent*)
          check_player_owns_continent h.parts
            (owned_to_terr_list p.terrs)
        then h.bonus + continent_bonus t p
        else continent_bonus t p

  (* [allocate_troops] calculates the number of troops a player has
     earned at start of round*)
  let allocate_troops (p : player) : int =
    let owned_bonus =
      if count_terrs p.terrs <= 9 + continent_bonus all_continents p
      then 3
      else (count_terrs p.terrs / 3) + continent_bonus all_continents p
    in
    if must_trade_in p.cards then owned_bonus + pattern_troops p.cards
    else owned_bonus

  let add_troops (p : player) (terr : territory) (num : int) : player =
    let terr_list = p.terrs in
    let terr_iq = to_owned_terr terr_list terr in
    let og_num = terr_iq.troops in
    let new_terr = { terr_iq with troops = og_num + num } in
    let new_list =
      new_terr :: List.filter (fun x -> x != terr_iq) terr_list
    in
    { p with terrs = new_list }
end

module Battle = struct
  open Player

  let roll_dice (die_input : int) : int list =
    if die_input = 3 then
      List.rev
        (List.fast_sort Stdlib.compare
           [ Random.int 6 + 1; Random.int 6 + 1; Random.int 6 + 1 ])
    else if die_input = 2 then
      List.rev
        (List.fast_sort Stdlib.compare
           [ Random.int 6 + 1; Random.int 6 + 1 ])
    else if die_input = 1 then [ Random.int 6 + 1 ]
    else []

  let territory_army_defense (terr : owned_territory) : int list =
    if terr.troops >= 2 then roll_dice 2 else roll_dice 1

  let territory_army_attack (terr : owned_territory) : int list =
    if terr.troops > 3 then roll_dice 3 else roll_dice (terr.troops - 1)

  let rec battle_simulator
      (terr_1 : owned_territory)
      (terr_2 : owned_territory) : owned_territory =
    if terr_1.troops <= 1 then terr_2
    else if terr_2.troops < 1 then terr_1
    else battle_simulator_help3 terr_1 terr_2

  and battle_simulator_help3 terr_1 terr_2 =
    let dice1 = territory_army_attack terr_1 in
    let dice2 = territory_army_defense terr_2 in

    match dice2 with
    | [] -> terr_1
    | [ d ] ->
        if d < List.hd dice1 then terr_1
        else
          battle_simulator
            { terr_1 with troops = terr_1.troops - 1 }
            terr_2
    | d1 :: d2 :: _ -> (
        match dice1 with
        | [] -> terr_2
        | [ a ] -> battle_simulator_help2 a d1 terr_2 terr_1
        | a1 :: a2 :: _ ->
            battle_simulator_help1 a1 d1 a2 d2 terr_1 terr_2)

  and battle_simulator_help2 a d1 terr_2 terr_1 =
    if a > d1 then
      battle_simulator terr_1 { terr_2 with troops = terr_2.troops - 1 }
    else
      battle_simulator { terr_1 with troops = terr_1.troops - 1 } terr_2

  and battle_simulator_help1 a1 d1 a2 d2 terr_1 terr_2 =
    if a1 > d1 && a2 > d2 then
      battle_simulator terr_1 { terr_2 with troops = terr_2.troops - 2 }
    else if a1 <= d1 && a2 <= d2 then
      battle_simulator { terr_1 with troops = terr_1.troops - 2 } terr_2
    else
      battle_simulator
        { terr_1 with troops = terr_1.troops - 1 }
        { terr_2 with troops = terr_2.troops - 1 }

  let make_list_bat2
      winner
      attacker_terr
      player_attacker
      loser
      player_defender
      defender_terr =
    [
      {
        player_attacker with
        terrs =
          List.filter
            (fun a -> a.place != attacker_terr.place)
            player_attacker.terrs
          @ [ { attacker_terr with troops = 1 } ];
      };
      {
        player_defender with
        terrs =
          List.filter
            (fun a -> a.place != defender_terr.place)
            player_defender.terrs
          @ [ winner ];
      };
    ]

  let make_list_bat
      winner
      attacker_terr
      player_attacker
      loser
      player_defender
      defender_terr =
    if winner.place = attacker_terr.place then
      [
        {
          player_attacker with
          terrs =
            { winner with troops = winner.troops - 1 }
            :: { loser with troops = 1 }
            :: List.filter
                 (fun a -> a.place != winner.place)
                 player_attacker.terrs;
        };
        {
          player_defender with
          terrs =
            List.filter
              (fun a -> a.place != loser.place)
              player_defender.terrs;
        };
      ]
      (* Defender wins. Both territory lists remain the same, except for
         updating the number of troops in the defenders 'winner'
         territory and attackers territory which goes to only 1 troop*)
    else
      make_list_bat2 winner attacker_terr player_attacker loser
        player_defender defender_terr

  let battle
      (player_attacker : player)
      (player_defender : player)
      (attacker_terr : owned_territory)
      (defender_terr : owned_territory) : player list =
    let winner = battle_simulator attacker_terr defender_terr in
    let loser =
      if winner.place = attacker_terr.place then defender_terr
      else attacker_terr
    in
    (* Attacker wins. Need to add 'winner' to attacker's territory list
       with updated troops, and remove it from defender's territory
       list.*)
    let list =
      make_list_bat winner attacker_terr player_attacker loser
        player_defender defender_terr
    in
    let attacker = List.nth list 0 in
    let attacker_cards = attacker.cards in
    let defender = List.nth list 1 in
    if lost defender then
      let free_cards = defender.cards in
      [
        { attacker with cards = attacker_cards @ free_cards };
        { defender with cards = [] };
      ]
    else list

  let rec odds
      (terr_1 : owned_territory)
      (terr_2 : owned_territory)
      (iterations : float)
      (constant_iter : float)
      (odd : float) : float =
    if iterations = 0. then odd /. constant_iter
    else if
      (battle_simulator terr_1 terr_2).place.name = terr_1.place.name
    then odds terr_1 terr_2 (iterations -. 1.) constant_iter (odd +. 1.)
    else odds terr_1 terr_2 (iterations -. 1.) constant_iter odd

  let odds_main
      (terr_1 : owned_territory)
      (terr_2 : owned_territory)
      (iterations : int) : float =
    odds terr_1 terr_2
      (float_of_int iterations)
      (float_of_int iterations)
      0.
end

module Fortify = struct
  open Player
  open Territories

  exception CantUseToFortify

  let rec to_terr (terr_list : owned_territory list) : territory list =
    match terr_list with
    | [] -> []
    | h :: t -> h.place :: to_terr t

  let rec filter_list (list : territory list) (acc : territory list) :
      territory list =
    match list with
    | [] -> acc
    | h :: t ->
        if List.mem h acc then filter_list t acc
        else
          let acc = h :: acc in
          filter_list t acc

  let rec filter_list (list : territory list) (acc : territory list) :
      territory list =
    match list with
    | [] -> acc
    | h :: t ->
        if List.mem h acc then filter_list t acc
        else
          let acc = h :: acc in
          filter_list t acc

  let rec c_fortify
      (p : player)
      (terr : territory list)
      (acc : territory list)
      stage =
    match terr with
    | [] -> acc
    | h :: t ->
        if List.mem h acc then c_fortify p t acc stage
        else if List.mem h (List.map (fun x -> x.place) p.terrs) then
          (h :: ca_fortify p h acc stage) @ c_fortify p t acc stage
        else c_fortify p t acc stage

  and ca_fortify
      (p : player)
      (terr : territory)
      (acc : territory list)
      stage : territory list =
    let borders =
      List.map (fun x -> to_territory x stage) terr.borders
    in
    c_fortify p borders (terr :: acc) stage

  let can_fortify p terr stage : territory list =
    let lst = filter_list (ca_fortify p terr [] stage) [] in
    List.filter (fun x -> x != terr) lst

  let fortify
      (curr_player : player)
      (troop : int)
      (terr1 : territory)
      (terr2 : territory)
      stage =
    let og_list = curr_player.terrs in
    let start = to_owned_terr og_list terr1 in
    let final = to_owned_terr og_list terr2 in
    if troop > start.troops - 1 then raise CantUseToFortify
    else
      let valid_fortify_list =
        filter_list (can_fortify curr_player terr1 stage) []
      in
      if List.mem terr2 valid_fortify_list then
        let new_terrs =
          let owned1 = to_owned_terr curr_player.terrs terr1 in
          let owned2 = to_owned_terr curr_player.terrs terr2 in
          [
            { owned1 with troops = owned1.troops - troop };
            { owned2 with troops = owned2.troops + troop };
          ]
        in
        let f x = not (x = start || x = final) in
        let new_list = List.filter f og_list @ new_terrs in
        { curr_player with terrs = new_list }
      else raise InvalidTerritory
end

module Init = struct
  open Player
  open Territories
  open Battle

  type gamestate = {
    p1 : player;
    p2 : player;
    p3 : player;
    p4 : player;
    state : int;
  }

  let init_players
      (p1 : string)
      (p2 : string)
      (p3 : string)
      (p4 : string) =
    [
      { name = p1; terrs = []; cards = [] };
      { name = p2; terrs = []; cards = [] };
      { name = p3; terrs = []; cards = [] };
      { name = p4; terrs = []; cards = [] };
    ]

  let rec shuffle_terrs_0
      (terrs : 'a list)
      (acc1 : 'a list)
      (acc2 : 'a list) =
    match terrs with
    | [] -> acc1 @ acc2
    | l1 :: l2 :: t -> shuffle_terrs_0 t (l1 :: acc1) (l2 :: acc2)
    | l1 :: t -> shuffle_terrs_0 t (l1 :: acc1) acc2

  let shuffle_terrs_0_main terrs = shuffle_terrs_0 terrs [] []

  let rec shuffle_terrs_1
      (terrs : 'a list)
      (acc1 : 'a list)
      (acc2 : 'a list) =
    match terrs with
    | [] -> acc1 @ acc2
    | l1 :: l2 :: t -> shuffle_terrs_1 t (l2 :: acc1) (l1 :: acc2)
    | l1 :: t -> shuffle_terrs_1 t (l1 :: acc1) acc2

  let shuffle_terrs_1_main terrs = shuffle_terrs_1 terrs [] []

  let rec shuffle_terrs_2 (terrs : 'a list) (acc : 'a list) =
    match terrs with
    | [] -> acc
    | l1 :: l2 :: t -> shuffle_terrs_2 t (l2 :: l1 :: acc)
    | l1 :: t -> shuffle_terrs_2 t (l1 :: acc)

  let shuffle_terrs_2_main terrs = shuffle_terrs_2 terrs []

  let rec shuffle_terrs_3 (terrs : 'a list) (acc : 'a list) =
    match terrs with
    | [] -> acc
    | l1 :: l2 :: t -> shuffle_terrs_3 t (acc @ [ l2 ] @ [ l1 ])
    | l1 :: t -> shuffle_terrs_3 t (acc @ [ l1 ])

  let shuffle_terrs_3_main terrs = shuffle_terrs_3 terrs []

  (*shuffles list*)
  let rec shuffle (terrs : 'a list) (acc : int) =
    if acc = 0 then terrs
    else
      shuffle
        (let rand = Random.int 4 in
         if rand = 0 then shuffle_terrs_0_main terrs
         else if rand = 1 then shuffle_terrs_1_main terrs
         else if rand = 2 then shuffle_terrs_2_main terrs
         else shuffle_terrs_3_main terrs)
        (acc - 1)

  let divide_terr_helper l1 l2 l3 l4 acc =
    match acc with
    | a1 :: a2 :: a3 :: a4 :: _ ->
        [ a1 @ l1; a2 @ l2; a3 @ l3; a4 @ l4 ]
    | _ -> [ [] ]

  let rec divide_terrs (terrs : 'a list) (acc : 'a list list) :
      'a list list =
    match terrs with
    | [] -> acc
    | l1 :: l2 :: l3 :: l4 :: t ->
        divide_terrs t
          (divide_terr_helper [ l1 ] [ l2 ] [ l3 ] [ l4 ] acc)
    | l1 :: l2 :: l3 :: t ->
        divide_terrs t (divide_terr_helper [ l1 ] [ l2 ] [ l3 ] [] acc)
    | l1 :: l2 :: t ->
        divide_terrs t (divide_terr_helper [ l1 ] [ l2 ] [] [] acc)
    | l1 :: t -> divide_terrs t (divide_terr_helper [ l1 ] [] [] [] acc)

  let divide_terrs_main (rand : int) (terrs : territory list) =
    divide_terrs (shuffle terrs rand) [ []; []; []; [] ]

  let rec addtroop (terrlist : territory list) : owned_territory list =
    match terrlist with
    | [] -> []
    | h :: t -> { place = h; troops = 1 } :: addtroop t

  let rec addtroopstoall (terrlists : territory list list) :
      owned_territory list list =
    match terrlists with
    | [] -> []
    | h :: t -> addtroop h :: addtroopstoall t

  let split_terrs_helper terrsplit =
    match terrsplit with
    | [] -> []
    | _ :: t -> t

  let rec split_terrs
      (game : player list)
      (terrsplit : owned_territory list list) : player list =
    match game with
    | [] -> []
    | h :: t ->
        { h with terrs = List.hd terrsplit }
        :: split_terrs t (split_terrs_helper terrsplit)

  let split_terrs_main (game : player list) rolls terrs : player list =
    split_terrs game (divide_terrs_main rolls terrs |> addtroopstoall)

  let rec add_1_to_nth
      (playerterrs : owned_territory list)
      (terrchanged : owned_territory) =
    match playerterrs with
    | [] -> []
    | h :: t ->
        if h = terrchanged then { h with troops = h.troops + 1 } :: t
        else h :: add_1_to_nth t terrchanged

  let rec randomly_distribute_troops
      (playerterrs : owned_territory list)
      (troops_left : int) =
    if troops_left = 0 then playerterrs
    else
      let ind = Random.int (List.length playerterrs) in
      randomly_distribute_troops
        (add_1_to_nth playerterrs (List.nth playerterrs ind))
        (troops_left - 1)

  let rec distribute_troops game =
    match game with
    | [] -> []
    | h :: t ->
        {
          h with
          terrs =
            randomly_distribute_troops h.terrs (35 - List.length h.terrs);
        }
        :: distribute_troops t

  let list_to_record (players : player list) =
    {
      p1 = List.nth players 0;
      p2 = List.nth players 1;
      p3 = List.nth players 2;
      p4 = List.nth players 3;
      state = 0;
    }

  let x input = Random.int input

  let fully_init_game
      (p1 : string)
      (p2 : string)
      (p3 : string)
      (p4 : string)
      rolls
      terrs : gamestate =
    list_to_record
      (distribute_troops
         (split_terrs_main (init_players p1 p2 p3 p4) rolls terrs))

  let battle_init_help
      attack_num
      defend_num
      game
      new_attacker
      new_defender =
    if attack_num = 3 then
      if defend_num = 1 then
        { game with p3 = new_attacker; p1 = new_defender }
      else if defend_num = 2 then
        { game with p3 = new_attacker; p2 = new_defender }
      else { game with p3 = new_attacker; p4 = new_defender }
    else if defend_num = 1 then
      { game with p4 = new_attacker; p1 = new_defender }
    else if defend_num = 2 then
      { game with p4 = new_attacker; p2 = new_defender }
    else { game with p4 = new_attacker; p3 = new_defender }

  let battle_init
      (game : gamestate)
      (attacker : player)
      (defender : player)
      (terr1 : owned_territory)
      (terr2 : owned_territory)
      (attack_num : int)
      (defend_num : int) : gamestate =
    let updated_players = battle attacker defender terr1 terr2 in
    let _ = Stdlib.print_string (List.hd updated_players).name in
    let new_attacker = List.nth updated_players 0 in
    let new_defender = List.nth updated_players 1 in
    if attack_num = 1 then
      if defend_num = 2 then
        { game with p1 = new_attacker; p2 = new_defender }
      else if defend_num = 3 then
        { game with p1 = new_attacker; p3 = new_defender }
      else { game with p1 = new_attacker; p4 = new_defender }
    else if attack_num = 2 then
      if defend_num = 1 then
        { game with p2 = new_attacker; p1 = new_defender }
      else if defend_num = 3 then
        { game with p2 = new_attacker; p3 = new_defender }
      else { game with p2 = new_attacker; p4 = new_defender }
    else
      battle_init_help attack_num defend_num game new_attacker
        new_defender

  let find_owner_of_terr game terr =
    if List.mem terr (name_list_owned game.p1.terrs) then game.p1
    else if List.mem terr (name_list_owned game.p2.terrs) then game.p2
    else if List.mem terr (name_list_owned game.p3.terrs) then game.p3
    else game.p4

  let rec list_available_borders game borders player =
    match borders with
    | [] -> ""
    | h :: t ->
        if List.mem h (name_list_owned player.terrs) then
          list_available_borders game t player
        else
          "\n" ^ h ^ " owned by: " ^ (find_owner_of_terr game h).name
          ^ "\n     number of troops: "
          ^ string_of_int
              (num_troops_in_terr h (find_owner_of_terr game h))
          ^ "\n"
          ^ list_available_borders game t player
end
