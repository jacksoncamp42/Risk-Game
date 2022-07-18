module Territories : sig
  type territory = {
    name : string;
    borders : string list;
  }
  (**A [territory] represents a country with a name and a list of
     bordering countries*)

  type continent = {
    parts : territory list;
    bonus : int;
  }
  (**A [continent] is a list of territories and contains the troop bonus
     value that anyone who holds the country recieves at the start of
     their turn*)

  val alaska : territory
  (**The territories [alaska], [northwest_territory], ... , [yakutsk]
     are all of the territories on the game board*)

  val northwest_territory : territory
  val alberta : territory
  val western_us : territory
  val central_america : territory
  val eastern_us : territory
  val ontario : territory
  val quebec : territory
  val greenland : territory
  val venezuela : territory
  val peru : territory
  val argentina : territory
  val brazil : territory
  val north_africa : territory
  val congo : territory
  val south_africa : territory
  val egypt : territory
  val madagascar : territory
  val east_africa : territory
  val western_europe : territory
  val great_britain : territory
  val iceland : territory
  val scandinavia : territory
  val northern_europe : territory
  val southern_europe : territory
  val ukraine : territory
  val indonesia : territory
  val new_guinea : territory
  val western_australia : territory
  val eastern_australia : territory
  val middle_east : territory
  val afghanistan : territory
  val china : territory
  val india : territory
  val irkutsk : territory
  val japan : territory
  val kamchatka : territory
  val mongolia : territory
  val siam : territory
  val siberia : territory
  val ural : territory
  val yakutsk : territory

  val asia : continent
  (**The continents in play on the game board as represented as [asia],
     ... , [north_america]*)

  val australia : continent
  val europe : continent
  val africa : continent
  val south_america : continent
  val north_america : continent

  val all_territories : territory list
  (**[all_territories] is a list of all of the territories*)

  val all_continents : continent list
  (**[all_continents] is a list of the continents*)

  val is_border : territory -> territory -> bool
  (**[is_border] says whether the two given territories border one
     another*)

  val is_territory : string -> bool
  (**[is_territory] says if [name] is the name of a valid territory*)

  exception InvalidTerritory

  val to_territory : string -> territory list -> territory
  (**[to_territory name list] converts a [name] to a territory. Raises
     InvalidTerritory exception if there is no territory with [name]*)

  val say_borders : territory -> string list
  (**[say_borders] lists the borders of [place]*)

  val sub_list : territory list -> territory list -> bool
  (**[sub_list] says whether player owns a given [continent_iq]*)

  val name_list_terrs : territory list -> string list
  (**[name_list_terrs] converts an territory list to a list of the names
     of the territories*)

  val continentsfake : territory list
  (**[continentsfake] is a list of all of territories in test map*)
end

module Cards : sig
  type card =
    | Troop
    | Horse
    | Cannon  (**A card can be a troop, a horse, or a cannon*)

  val card_count : card list -> int list
  (**[card_count] gives a count of each type of card that [cards] has,
     with the order being troops, horses, and cannons*)

  val has_pattern : card list -> bool
  (**[has_pattern] returns whether [cards] has a pattern*)

  val pattern_troops : card list -> int
  (**[pattern_troops] gives the troop bonus that [cards] produces*)

  val must_trade_in : card list -> bool
  (**[must_trade_in] says if a player must trade in their [cards]*)

  val remove_cards : card list -> int -> card list
  (**[remove_cards] takes cards from a card list depending on the amount
     of their bonus*)
end

module Player : sig
  open Territories
  open Cards

  type owned_territory = {
    place : territory;
    troops : int;
  }
  (**An owned_territory contains a given territory and the troops in it*)

  type player = {
    name : string;
    terrs : owned_territory list;
    cards : card list;
  }
  (**A player has a name, a list of territories they own, and a list of
     their cards*)

  val num_troops_in_terr : string -> player -> int
  (**[num_troops_in_terr terr p] gives troops in terr owned by player p*)

  val to_owned_terr :
    owned_territory list -> territory -> owned_territory
  (**[to_owned_terr lst terr] returns the owned_territory of a given
     territory that a player owns [terr]*)

  val terrs_to_string : owned_territory list -> string
  (**[terrs_to_string lst] returns the owned territories list [lst] in a
     string with | in between*)

  val name_list_owned : owned_territory list -> string list
  (**[name_list_owned lst] converts an owned_territory list [lst] to a
     list of the names of the territories*)

  val list_of_available_borders : string list -> player -> string list
  (**[name_list lst p] makes a list of the borders of a territory that
     the current player can attack based on the borders [lst] and player
     [p]*)

  val winner : player -> territory list -> bool
  (**[winner p lst] says whether the given player [p] has won the game
     based on their list of territories [lst]*)

  val lost : player -> bool
  (**[lost p] says whether the given player [p] has lost*)

  val show_cards : player -> string
  (**[show_cards p] gives the string representation of a player [p]'s
     cards*)

  val give_card : player -> player
  (**[give_card p] gives a player [p] a random card*)
end

module Draft : sig
  open Player
  open Territories
  open Cards

  val count_terrs : owned_territory list -> int
  (**[count_terrs lst] counts the number of territories in [lst]*)

  val continent_bonus : continent list -> player -> int
  (**[continent_bonus lst p] gives the amount of bonus troops a player
     [p] get for holding continents based on [lst]*)

  val allocate_troops : player -> int
  (**[allocate_troops p] gives the amount of troops that a player [p]
     gets to allocate to their territories at the start of their turn*)

  val add_troops : player -> territory -> int -> player
  (**[add_troops p terr num] adds given number [num] of troops to a
     territory [terr] owned by player [p]*)
end

module Battle : sig
  open Player

  val battle_simulator :
    owned_territory -> owned_territory -> owned_territory
  (**[battle_simulator terr1 terr2] produces the winning country when a
     battle between [terr1] and [terr2] occurs, and it updates the
     amount of troops the winning country has*)

  val odds_main : owned_territory -> owned_territory -> int -> float
  (**[odds_main terr1 terr2] is the chance of the attacking territory
     winning the battle in a battle between [terr1] and [terr2] This is
     mainly intended for testing purposes*)
end

module Fortify : sig
  open Player
  open Territories

  exception CantUseToFortify

  val to_terr : owned_territory list -> territory list
  (**[to_terr lst] converts a player's owned_territory list [lst] to a
     territory list*)

  val can_fortify :
    player -> territory -> territory list -> territory list
  (**[can_fortify p terr lst] is the list of territories that a player
     [p] can fortify fromm territory [terr]*)

  val fortify :
    player -> int -> territory -> territory -> territory list -> player
  (**[fortify p troop terr1 terr2] moves a given number of troops
     ([troop]) from one territory [terr1] to another [terr2] in the
     fortify phase. [troop] must be greater than 1. Raises
     CantUseToFortify if [terr1] cannot provide troops. Raises
     InvalidTerritory if a territory cannot be fortified*)
end

module Init : sig
  open Player
  open Territories

  type gamestate = {
    p1 : player;
    p2 : player;
    p3 : player;
    p4 : player;
    state : int;
  }

  val fully_init_game :
    string ->
    string ->
    string ->
    string ->
    int ->
    territory list ->
    gamestate

  val battle_init :
    gamestate ->
    player ->
    player ->
    owned_territory ->
    owned_territory ->
    int ->
    int ->
    gamestate

  val list_available_borders :
    gamestate -> string list -> player -> string
  (**[name_list game lst p] lists the borders of a territory that the
     current player [p] can attack in game [game]*)

  val find_owner_of_terr : gamestate -> string -> player
  (**[find_owner_of_terr game terr] finds the player who owns terr in
     game*)
end