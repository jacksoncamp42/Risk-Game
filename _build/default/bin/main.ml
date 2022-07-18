open Risk
open Init
open Player
open Draft
open Territories
open Fortify
open Bogue
module I = Image
module L = Layout
module W = Widget
module S = Style
module T = Trigger

type maps = {
  name : string;
  list : territory list;
}

let real_board = { name = "full board"; list = all_territories }
let fake_board = { name = "test board"; list = continentsfake }

let t_on_terr terr game =
  string_of_int (num_troops_in_terr terr (find_owner_of_terr game terr))

let p_on_terr terr game = (find_owner_of_terr game terr).name

let main (game : gamestate) =
  let image = W.image ~w:1200 ~h:709 "risk-board.png" in
  let image = L.flat ~name:"image" [ L.resident image ] in
  let greenland = W.text_display ~w:100 "Greenland" |> L.resident in
  let green_tr =
    W.label ~align:Center
      ("P:"
      ^ p_on_terr "greenland" game
      ^ "; "
      ^ t_on_terr "greenland" game)
    |> L.resident
  in
  let alaska = W.text_display ~w:100 "Alaska" |> L.resident in
  let alaska_tr =
    W.label ~align:Center
      ("P:" ^ p_on_terr "alaska" game ^ "; " ^ t_on_terr "alaska" game)
    |> L.resident
  in
  let northwest =
    W.text_display ~w:100 "Northwest Territory" |> L.resident
  in
  let northwest_tr =
    W.label ~align:Center
      ("P:"
      ^ p_on_terr "northwest territory" game
      ^ "; "
      ^ t_on_terr "northwest territory" game)
    |> L.resident
  in
  let alberta = W.text_display ~w:100 "Alberta" |> L.resident in
  let alberta_tr =
    W.label ~align:Center
      ("P:" ^ p_on_terr "alberta" game ^ "; " ^ t_on_terr "alberta" game)
    |> L.resident
  in
  let ontario = W.text_display ~w:100 "Ontario" |> L.resident in
  let ontario_tr =
    W.label ~align:Center
      ("P:" ^ p_on_terr "ontario" game ^ "; " ^ t_on_terr "ontario" game)
    |> L.resident
  in
  let quebec = W.text_display ~w:100 "Quebec" |> L.resident in
  let quebec_tr =
    W.label ~align:Center
      ("P:" ^ p_on_terr "quebec" game ^ "; " ^ t_on_terr "quebec" game)
    |> L.resident
  in
  let western_us =
    W.text_display ~w:100 "Western United States" |> L.resident
  in
  let western_us_tr =
    W.label ~align:Center
      ("P:"
      ^ p_on_terr "western us" game
      ^ "; "
      ^ t_on_terr "western us" game)
    |> L.resident
  in
  let eastern_us =
    W.text_display ~w:100 "Eastern United States" |> L.resident
  in
  let eastern_us_tr =
    W.label ~align:Center
      ("P:"
      ^ p_on_terr "eastern us" game
      ^ "; "
      ^ t_on_terr "eastern us" game)
    |> L.resident
  in
  let central_america =
    W.text_display ~w:100 "Central America" |> L.resident
  in
  let central_america_tr =
    W.label ~align:Center
      ("P:"
      ^ p_on_terr "central america" game
      ^ "; "
      ^ t_on_terr "central america" game)
    |> L.resident
  in
  let venezuela = W.text_display ~w:100 "Venezuela" |> L.resident in
  let venezuela_tr =
    W.label ~align:Center
      ("P:"
      ^ p_on_terr "venezuela" game
      ^ "; "
      ^ t_on_terr "venezuela" game)
    |> L.resident
  in
  let peru = W.text_display ~w:100 "Peru" |> L.resident in
  let peru_tr =
    W.label ~align:Center
      ("P:" ^ p_on_terr "peru" game ^ "; " ^ t_on_terr "peru" game)
    |> L.resident
  in
  let brazil = W.text_display ~w:100 "Brazil" |> L.resident in
  let brazil_tr =
    W.label ~align:Center
      ("P:" ^ p_on_terr "brazil" game ^ "; " ^ t_on_terr "brazil" game)
    |> L.resident
  in
  let argentina = W.text_display ~w:100 "Argentina" |> L.resident in
  let argentina_tr =
    W.label ~align:Center
      ("P:"
      ^ p_on_terr "argentina" game
      ^ "; "
      ^ t_on_terr "argentina" game)
    |> L.resident
  in
  let scandinavia = W.text_display ~w:100 "Scandinavia" |> L.resident in
  let scandinavia_tr =
    W.label ~align:Center
      ("P:"
      ^ p_on_terr "scandinavia" game
      ^ "; "
      ^ t_on_terr "scandinavia" game)
    |> L.resident
  in
  let iceland = W.text_display ~w:100 "Iceland" |> L.resident in
  let iceland_tr =
    W.label ~align:Center
      ("P:" ^ p_on_terr "iceland" game ^ "; " ^ t_on_terr "iceland" game)
    |> L.resident
  in
  let great_britain =
    W.text_display ~w:100 "Great Britain" |> L.resident
  in
  let great_britain_tr =
    W.label ~align:Center
      ("P:"
      ^ p_on_terr "great britain" game
      ^ "; "
      ^ t_on_terr "great britain" game)
    |> L.resident
  in
  let ukraine = W.text_display ~w:100 "Ukraine" |> L.resident in
  let ukraine_tr =
    W.label ~align:Center
      ("P:" ^ p_on_terr "ukraine" game ^ "; " ^ t_on_terr "ukraine" game)
    |> L.resident
  in
  let north_europe =
    W.text_display ~w:100 "Northern Europe" |> L.resident
  in
  let north_europe_tr =
    W.label ~align:Center
      ("P:"
      ^ p_on_terr "northern europe" game
      ^ "; "
      ^ t_on_terr "northern europe" game)
    |> L.resident
  in
  let south_europe =
    W.text_display ~w:100 "Southern Europe" |> L.resident
  in
  let south_europe_tr =
    W.label ~align:Center
      ("P:"
      ^ p_on_terr "southern europe" game
      ^ "; "
      ^ t_on_terr "southern europe" game)
    |> L.resident
  in
  let west_europe =
    W.text_display ~w:100 "Western Europe" |> L.resident
  in
  let west_europe_tr =
    W.label ~align:Center
      ("P:"
      ^ p_on_terr "western europe" game
      ^ "; "
      ^ t_on_terr "western europe" game)
    |> L.resident
  in
  let egypt = W.text_display ~w:100 "Egypt" |> L.resident in
  let egypt_tr =
    W.label ~align:Center
      ("P:" ^ p_on_terr "egypt" game ^ "; " ^ t_on_terr "egypt" game)
    |> L.resident
  in
  let north_africa =
    W.text_display ~w:100 "North Africa" |> L.resident
  in
  let north_africa_tr =
    W.label ~align:Center
      ("P:"
      ^ p_on_terr "north africa" game
      ^ "; "
      ^ t_on_terr "north africa" game)
    |> L.resident
  in
  let congo = W.text_display ~w:100 "Congo" |> L.resident in
  let congo_tr =
    W.label ~align:Center
      ("P:" ^ p_on_terr "congo" game ^ "; " ^ t_on_terr "congo" game)
    |> L.resident
  in
  let east_africa = W.text_display ~w:100 "East Africa" |> L.resident in
  let east_africa_tr =
    W.label ~align:Center
      ("P:"
      ^ p_on_terr "east africa" game
      ^ "; "
      ^ t_on_terr "east africa" game)
    |> L.resident
  in
  let south_africa =
    W.text_display ~w:100 "South Africa" |> L.resident
  in
  let south_africa_tr =
    W.label ~align:Center
      ("P:"
      ^ p_on_terr "south africa" game
      ^ "; "
      ^ t_on_terr "south africa" game)
    |> L.resident
  in
  let madagascar = W.text_display ~w:100 "Madagascar" |> L.resident in
  let madagascar_tr =
    W.label ~align:Center
      ("P:"
      ^ p_on_terr "madagascar" game
      ^ "; "
      ^ t_on_terr "madagascar" game)
    |> L.resident
  in
  let yakutsk = W.text_display ~w:100 "Yakutsk" |> L.resident in
  let yakutsk_tr =
    W.label ~align:Center
      ("P:" ^ p_on_terr "yakutsk" game ^ "; " ^ t_on_terr "yakutsk" game)
    |> L.resident
  in
  let kamchatka = W.text_display ~w:100 "Kamchatka" |> L.resident in
  let kamchatka_tr =
    W.label ~align:Center
      ("P:"
      ^ p_on_terr "kamchatka" game
      ^ "; "
      ^ t_on_terr "kamchatka" game)
    |> L.resident
  in
  let irkutsk = W.text_display ~w:100 "Irkutsk" |> L.resident in
  let irkutsk_tr =
    W.label ~align:Center
      ("P:" ^ p_on_terr "irkutsk" game ^ "; " ^ t_on_terr "irkutsk" game)
    |> L.resident
  in
  let mongolia = W.text_display ~w:100 "Mongolia" |> L.resident in
  let mongolia_tr =
    W.label ~align:Center
      ("P:"
      ^ p_on_terr "mongolia" game
      ^ "; "
      ^ t_on_terr "mongolia" game)
    |> L.resident
  in
  let japan = W.text_display ~w:100 "Japan" |> L.resident in
  let japan_tr =
    W.label ~align:Center
      ("P:" ^ p_on_terr "japan" game ^ "; " ^ t_on_terr "japan" game)
    |> L.resident
  in
  let siberia = W.text_display ~w:100 "Siberia" |> L.resident in
  let siberia_tr =
    W.label ~align:Center
      ("P:" ^ p_on_terr "siberia" game ^ "; " ^ t_on_terr "siberia" game)
    |> L.resident
  in
  let ural = W.text_display ~w:100 "Ural" |> L.resident in
  let ural_tr =
    W.label ~align:Center
      ("P:" ^ p_on_terr "ural" game ^ "; " ^ t_on_terr "ural" game)
    |> L.resident
  in
  let afghanistan = W.text_display ~w:100 "Afghanistan" |> L.resident in
  let afghanistan_tr =
    W.label ~align:Center
      ("P:"
      ^ p_on_terr "afghanistan" game
      ^ "; "
      ^ t_on_terr "afghanistan" game)
    |> L.resident
  in
  let china = W.text_display ~w:100 "China" |> L.resident in
  let china_tr =
    W.label ~align:Center
      ("P:" ^ p_on_terr "china" game ^ "; " ^ t_on_terr "china" game)
    |> L.resident
  in
  let india = W.text_display ~w:100 "India" |> L.resident in
  let india_tr =
    W.label ~align:Center
      ("P:" ^ p_on_terr "india" game ^ "; " ^ t_on_terr "india" game)
    |> L.resident
  in
  let siam = W.text_display ~w:100 "Siam" |> L.resident in
  let siam_tr =
    W.label ~align:Center
      ("P:" ^ p_on_terr "siam" game ^ "; " ^ t_on_terr "siam" game)
    |> L.resident
  in
  let middle_east = W.text_display ~w:100 "Middle East" |> L.resident in
  let middle_east_tr =
    W.label ~align:Center
      ("P:"
      ^ p_on_terr "middle east" game
      ^ "; "
      ^ t_on_terr "middle east" game)
    |> L.resident
  in
  let indonesia = W.text_display ~w:100 "Indonesia" |> L.resident in
  let indonesia_tr =
    W.label ~align:Center
      ("P:"
      ^ p_on_terr "indonesia" game
      ^ "; "
      ^ t_on_terr "indonesia" game)
    |> L.resident
  in
  let new_guinea = W.text_display ~w:100 "New Guinea" |> L.resident in
  let new_guinea_tr =
    W.label ~align:Center
      ("P:"
      ^ p_on_terr "new guinea" game
      ^ "; "
      ^ t_on_terr "new guinea" game)
    |> L.resident
  in
  let eastern_australia =
    W.text_display ~w:100 "Eastern Australia" |> L.resident
  in
  let eastern_australia_tr =
    W.label ~align:Center
      ("P:"
      ^ p_on_terr "eastern australia" game
      ^ "; "
      ^ t_on_terr "eastern australia" game)
    |> L.resident
  in
  let western_australia =
    W.text_display ~w:100 "Western Australia" |> L.resident
  in
  let western_australia_tr =
    W.label ~align:Center
      ("P:"
      ^ p_on_terr "western australia" game
      ^ "; "
      ^ t_on_terr "western australia" game)
    |> L.resident
  in

  let layout =
    L.superpose
      [
        image;
        greenland;
        green_tr;
        alaska;
        alaska_tr;
        northwest;
        northwest_tr;
        alberta;
        alberta_tr;
        ontario;
        ontario_tr;
        quebec;
        quebec_tr;
        western_us;
        western_us_tr;
        eastern_us;
        eastern_us_tr;
        central_america;
        central_america_tr;
        venezuela;
        venezuela_tr;
        peru;
        peru_tr;
        brazil;
        brazil_tr;
        argentina;
        argentina_tr;
        scandinavia;
        scandinavia_tr;
        iceland;
        iceland_tr;
        great_britain;
        great_britain_tr;
        ukraine;
        ukraine_tr;
        north_europe;
        north_europe_tr;
        south_europe;
        south_europe_tr;
        west_europe;
        west_europe_tr;
        egypt;
        egypt_tr;
        north_africa;
        north_africa_tr;
        congo;
        congo_tr;
        east_africa;
        east_africa_tr;
        south_africa;
        south_africa_tr;
        madagascar;
        madagascar_tr;
        yakutsk;
        yakutsk_tr;
        irkutsk;
        irkutsk_tr;
        kamchatka;
        kamchatka_tr;
        japan;
        japan_tr;
        mongolia;
        mongolia_tr;
        siberia;
        siberia_tr;
        ural;
        ural_tr;
        afghanistan;
        afghanistan_tr;
        china;
        china_tr;
        siam;
        siam_tr;
        india;
        india_tr;
        middle_east;
        middle_east_tr;
        new_guinea;
        new_guinea_tr;
        indonesia;
        indonesia_tr;
        eastern_australia;
        eastern_australia_tr;
        western_australia;
        western_australia_tr;
      ]
  in

  L.setx greenland 400;
  L.sety greenland 70;
  L.setx green_tr 425;
  L.sety green_tr 90;
  L.setx alaska 85;
  L.sety alaska 100;
  L.setx alaska_tr 105;
  L.sety alaska_tr 120;
  L.setx northwest 160;
  L.sety northwest 100;
  L.setx northwest_tr 180;
  L.sety northwest_tr 130;
  L.setx alberta 170;
  L.sety alberta 160;
  L.setx alberta_tr 190;
  L.sety alberta_tr 180;
  L.setx ontario 250;
  L.sety ontario 170;
  L.setx ontario_tr 270;
  L.sety ontario_tr 190;
  L.setx quebec 330;
  L.sety quebec 170;
  L.setx quebec_tr 350;
  L.sety quebec_tr 190;
  L.setx western_us 170;
  L.sety western_us 220;
  L.setx western_us_tr 190;
  L.sety western_us_tr 250;
  L.setx eastern_us 270;
  L.sety eastern_us 240;
  L.setx eastern_us_tr 290;
  L.sety eastern_us_tr 270;
  L.setx central_america 115;
  L.sety central_america 300;
  L.setx central_america_tr 135;
  L.sety central_america_tr 330;
  L.setx venezuela 270;
  L.sety venezuela 380;
  L.setx venezuela_tr 290;
  L.sety venezuela_tr 400;
  L.setx peru 260;
  L.sety peru 450;
  L.setx peru_tr 280;
  L.sety peru_tr 470;
  L.setx brazil 360;
  L.sety brazil 450;
  L.setx brazil_tr 370;
  L.sety brazil_tr 470;
  L.setx argentina 300;
  L.sety argentina 530;
  L.setx argentina_tr 320;
  L.sety argentina_tr 550;
  L.setx scandinavia 600;
  L.sety scandinavia 120;
  L.setx scandinavia_tr 620;
  L.sety scandinavia_tr 140;
  L.setx iceland 500;
  L.sety iceland 130;
  L.setx iceland_tr 520;
  L.sety iceland_tr 150;
  L.setx great_britain 480;
  L.sety great_britain 210;
  L.setx great_britain_tr 500;
  L.sety great_britain_tr 230;
  L.setx ukraine 680;
  L.sety ukraine 200;
  L.setx ukraine_tr 700;
  L.sety ukraine_tr 220;
  L.setx north_europe 580;
  L.sety north_europe 220;
  L.setx north_europe_tr 600;
  L.sety north_europe_tr 240;
  L.setx south_europe 590;
  L.sety south_europe 290;
  L.setx south_europe_tr 610;
  L.sety south_europe_tr 310;
  L.setx west_europe 510;
  L.sety west_europe 280;
  L.setx west_europe_tr 530;
  L.sety west_europe_tr 300;
  L.setx egypt 640;
  L.sety egypt 380;
  L.setx egypt_tr 660;
  L.sety egypt_tr 400;
  L.setx north_africa 540;
  L.sety north_africa 420;
  L.setx north_africa_tr 560;
  L.sety north_africa_tr 440;
  L.setx east_africa 680;
  L.sety east_africa 460;
  L.setx east_africa_tr 700;
  L.sety east_africa_tr 480;
  L.setx congo 640;
  L.sety congo 500;
  L.setx congo_tr 660;
  L.sety congo_tr 520;
  L.setx south_africa 630;
  L.sety south_africa 590;
  L.setx south_africa_tr 650;
  L.sety south_africa_tr 610;
  L.setx madagascar 760;
  L.sety madagascar 590;
  L.setx madagascar_tr 780;
  L.sety madagascar_tr 610;
  L.setx ural 810;
  L.sety ural 160;
  L.setx ural_tr 830;
  L.sety ural_tr 180;
  L.setx siberia 860;
  L.sety siberia 100;
  L.setx siberia_tr 880;
  L.sety siberia_tr 120;
  L.setx yakutsk 960;
  L.sety yakutsk 80;
  L.setx yakutsk_tr 980;
  L.sety yakutsk_tr 100;
  L.setx irkutsk 950;
  L.sety irkutsk 160;
  L.setx irkutsk_tr 970;
  L.sety irkutsk_tr 180;
  L.setx mongolia 960;
  L.sety mongolia 230;
  L.setx mongolia_tr 980;
  L.sety mongolia_tr 250;
  L.setx kamchatka 1030;
  L.sety kamchatka 100;
  L.setx kamchatka_tr 1050;
  L.sety kamchatka_tr 120;
  L.setx japan 1080;
  L.sety japan 240;
  L.setx japan_tr 1100;
  L.sety japan_tr 260;
  L.setx china 910;
  L.sety china 300;
  L.setx china_tr 920;
  L.sety china_tr 320;
  L.setx afghanistan 780;
  L.sety afghanistan 260;
  L.setx afghanistan_tr 800;
  L.sety afghanistan_tr 280;
  L.setx middle_east 700;
  L.sety middle_east 350;
  L.setx middle_east_tr 720;
  L.sety middle_east_tr 370;
  L.setx india 850;
  L.sety india 330;
  L.setx india_tr 850;
  L.sety india_tr 350;
  L.setx siam 950;
  L.sety siam 380;
  L.setx siam_tr 950;
  L.sety siam_tr 400;
  L.setx indonesia 950;
  L.sety indonesia 480;
  L.setx indonesia_tr 955;
  L.sety indonesia_tr 500;
  L.setx new_guinea 1050;
  L.sety new_guinea 470;
  L.setx new_guinea_tr 1060;
  L.sety new_guinea_tr 490;
  L.setx eastern_australia 1090;
  L.sety eastern_australia 550;
  L.setx eastern_australia_tr 1110;
  L.sety eastern_australia_tr 580;
  L.setx western_australia 1000;
  L.sety western_australia 590;
  L.setx western_australia_tr 1020;
  L.sety western_australia_tr 620;

  let board = Bogue.make [] [ layout ] in
  Bogue.run board

let is_owner game num terr stage : bool =
  if num = 1 then
    List.mem
      (to_territory terr stage)
      (List.map (fun x -> x.place) game.p1.terrs)
  else if num = 2 then
    List.mem
      (to_territory terr stage)
      (List.map (fun x -> x.place) game.p2.terrs)
  else if num = 3 then
    List.mem
      (to_territory terr stage)
      (List.map (fun x -> x.place) game.p3.terrs)
  else
    List.mem
      (to_territory terr stage)
      (List.map (fun x -> x.place) game.p4.terrs)

let parse_names str =
  List.filter
    (fun x -> if x = "" then false else true)
    (String.split_on_char ' ' str)

let player_playing (game : gamestate) : player =
  if game.state = 0 then game.p1
  else if game.state = 1 then game.p2
  else if game.state = 2 then game.p3
  else game.p4

let player_playing_num (game : gamestate) (p : player) : int =
  if p.name = game.p1.name then 1
  else if p.name = game.p2.name then 2
  else if p.name = game.p3.name then 3
  else 4

let find_owner game terr =
  if List.mem terr (name_list_owned game.p1.terrs) then game.p1
  else if List.mem terr (name_list_owned game.p2.terrs) then game.p2
  else if List.mem terr (name_list_owned game.p3.terrs) then game.p3
  else game.p4

let print_fortify_stage (player : player) game =
  ANSITerminal.print_string [] "\nFortify Stage\n";
  print_string ("\nPlayer: " ^ player.name);
  print_string
    ("\nTerritories and Troops:\n"
    ^ terrs_to_string (player_playing game).terrs);
  print_string
    "\n\
     Enter which territory you want to move troops from or skip to \
     next player's turn";
  print_string "\n>";
  ()

let print_fortifying game terr stage player =
  ANSITerminal.print_string []
    ("\nBorders:\n"
    ^ String.concat "\n"
        (name_list_terrs (can_fortify player terr stage)));
  print_string "\nEnter which border to fortify or say back";
  print_string "\n>";
  ()

let print_troop_fortify game =
  ANSITerminal.print_string []
    "\nEnter how many troops you want to fortify or back";
  print_string "\n>";
  ()

let print_battle_stage (player : player) game =
  ANSITerminal.print_string [] "\nBattle Stage\n";
  print_string ("\nPlayer: " ^ player.name);
  print_string
    ("\nTerritories and Troops:\n"
    ^ terrs_to_string (player_playing game).terrs);
  print_string
    "\n\
     Enter which territory you want to attack with or skip to fortify \
     stage";
  print_string "\n>";
  ()

let print_attacking game player terr stage =
  print_string "\nBorders:\n";
  ANSITerminal.print_string []
    (list_available_borders game (to_territory terr stage).borders
       player);
  print_string "\nEnter which border to attack or say back";
  print_string "\n>";
  ()

let place_stage_print game troops_left (player : player) =
  ANSITerminal.print_string [] "\nPlace Troops Stage\n";
  print_string ("\nPlayer: " ^ game.p1.name);
  print_string
    ("\nTerritories and Troops:\n" ^ terrs_to_string game.p1.terrs);
  print_string ("\nPlayer: " ^ game.p2.name);
  print_string
    ("\nTerritories and Troops:\n" ^ terrs_to_string game.p2.terrs);
  print_string ("\nPlayer: " ^ game.p3.name);
  print_string
    ("\nTerritories and Troops:\n" ^ terrs_to_string game.p3.terrs);
  print_string ("\nPlayer: " ^ game.p4.name);
  print_string
    ("\nTerritories and Troops:\n" ^ terrs_to_string game.p4.terrs);
  print_string ("\nCurrent Player: " ^ player.name);
  print_string "\nYou have this many of each card:";
  let ccount = Cards.card_count player.cards in
  print_string ("\nInfantry: " ^ string_of_int (List.nth ccount 0));
  print_string ("\nCalvary: " ^ string_of_int (List.nth ccount 1));
  print_string
    ("\nArtillery: " ^ string_of_int (List.nth ccount 2) ^ "\n");
  print_string
    ("\nYou have " ^ string_of_int troops_left ^ " troops to place\n");
  ()

let rec fortify_stage (game : gamestate) stage : unit =
  let player = player_playing game in
  print_fortify_stage player game;
  match read_line () with
  | "skip" ->
      place_stage
        { game with state = (game.state + 1) mod 4 }
        (allocate_troops
           (player_playing { game with state = (game.state + 1) mod 4 }))
        stage
  | "quit" -> exit 0
  | "map" ->
      main game;
      fortify_stage game stage
  | x ->
      let y =
        try to_territory x stage with
        | InvalidTerritory -> { name = "fake"; borders = [] }
      in
      fortify_stage_help y game stage player

and fortify_stage_help x game stage player =
  let _ =
    if x.name = "fake" then (
      print_string "\nYou done fucked up try again\n";
      fortify_stage game stage)
    else print_string ""
  in
  if List.length (can_fortify player x stage) = 0 then (
    print_string "No borders to fortify";
    fortify_stage game stage)
  else if
    List.mem x
      (List.map
         (fun y -> to_territory y stage)
         (name_list_owned player.terrs))
  then fortifying game x stage
  else (
    print_string "\nYou done fucked up try again\n";
    fortify_stage game stage)

and fortifying game terr stage =
  let player = player_playing game in
  if (to_owned_terr player.terrs terr).troops = 1 then (
    print_string "\nYou done fucked up try again\n";
    fortify_stage game stage)
  else print_fortifying game terr stage player;
  let x = read_line () in
  if x = "back" then fortify_stage game stage
  else if List.mem x (name_list_terrs (can_fortify player terr stage))
  then troop_fortify game terr x stage
  else if x = "map" then (
    main game;
    fortifying game terr stage)
  else (
    print_string "\nYou done fucked up try again\n";
    fortifying game terr stage)

and troop_fortify game terr1 terr2 stage =
  let player = player_playing game in
  print_troop_fortify game;
  let x = read_line () in
  if x = "back" then fortify_stage game stage
  else if (to_owned_terr player.terrs terr1).troops > int_of_string x
  then
    let gamestate = game.state + 1 in
    let newp =
      fortify player (int_of_string x) terr1
        (to_territory terr2 stage)
        stage
    in
    troop_fortify_help game gamestate newp stage
  else if x = "map" then (
    main game;
    troop_fortify game terr1 terr2 stage)
  else (
    print_string "\nYou done fucked up try again\n";
    troop_fortify game terr1 terr2 stage)

and troop_fortify_help game gamestate newp stage =
  if gamestate = 1 then
    place_stage
      { game with state = (game.state + 1) mod 4; p1 = newp }
      (allocate_troops game.p2)
      stage
  else if gamestate = 2 then
    place_stage
      { game with state = (game.state + 1) mod 4; p2 = newp }
      (allocate_troops game.p3)
      stage
  else if gamestate = 3 then
    place_stage
      { game with state = (game.state + 1) mod 4; p3 = newp }
      (allocate_troops game.p4)
      stage
  else
    place_stage
      { game with state = (game.state + 1) mod 4; p4 = newp }
      (allocate_troops game.p1)
      stage

and battle_stage (game : gamestate) card stage : unit =
  let player = player_playing game in
  if winner player stage then
    let _ =
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        (player.name ^ " wins!!!" ^ "\n")
    in
    exit 0
  else print_battle_stage player game;
  match read_line () with
  | x ->
      if x = "skip" then fortify_stage game stage
      else if List.mem x (name_list_owned player.terrs) then
        attacking game x card stage
      else if x = "quit" then exit 0
      else if x = "map" then (
        main game;
        battle_stage game card stage)
      else (
        print_string "\nYou done fucked up try again\n";
        battle_stage game card stage)

and attacking (game : gamestate) (terr : string) (card : bool) stage =
  let player = player_playing game in
  if (to_owned_terr player.terrs (to_territory terr stage)).troops = 1
  then (
    print_string "\nYou done fucked up try again\n";
    battle_stage game card stage)
  else print_attacking game player terr stage;
  let x = read_line () in
  if x = "back" then battle_stage game card stage
  else if
    List.mem x
      (list_of_available_borders (to_territory terr stage).borders
         player)
  then attacking_help game player x stage terr card
  else if x = "map" then (
    main game;
    attacking game terr card stage)
  else (
    print_string "\nYou done fucked up try again\n";
    attacking game terr card stage)

and attacking_help game player x stage terr card =
  let new_game = new_game_help2 game player x stage terr in
  if is_owner new_game (player_playing_num new_game player) x stage then (
    ANSITerminal.print_string [ ANSITerminal.green ] "\nYou Won!\n";
    if card then battle_stage new_game true stage
    else
      let new_player = new_player_help game player new_game in
      let new_game = new_game_help game player new_game new_player in
      battle_stage new_game true stage)
  else (
    ANSITerminal.print_string [ ANSITerminal.red ] "You Lost!\n";
    battle_stage new_game card stage)

and new_player_help game player new_game =
  if player_playing_num game player = 1 then new_game.p1
  else if player_playing_num game player = 2 then new_game.p2
  else if player_playing_num game player = 3 then new_game.p3
  else new_game.p4

and new_game_help game player new_game new_player =
  if player_playing_num game player = 1 then
    { new_game with p1 = give_card new_player }
  else if player_playing_num game player = 2 then
    { new_game with p2 = give_card new_player }
  else if player_playing_num game player = 3 then
    { new_game with p3 = give_card new_player }
  else { new_game with p4 = give_card new_player }

and new_game_help2 game player x stage terr =
  battle_init game player (find_owner game x)
    (to_owned_terr player.terrs (to_territory terr stage))
    (to_owned_terr (find_owner game x).terrs (to_territory x stage))
    (player_playing_num game player)
    (player_playing_num game (find_owner game x))

and place_stage_help4 game stage =
  place_stage
    { game with state = (game.state + 1) mod 4 }
    (if game.state = 1 then allocate_troops game.p2
    else if game.state = 2 then allocate_troops game.p3
    else if game.state = 3 then allocate_troops game.p4
    else allocate_troops game.p1)
    stage

and place_stage_help5 (player : player) =
  let _ =
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      (player.name ^ " wins!!!")
  in
  exit 0

and placehelp6 game player pnum troops_left stage =
  print_string
    "\n\
     You must trade in your cards. Type trade to exchange cards for \
     troops";
  print_string "\n>";
  match read_line () with
  | "trade" ->
      let new_cards =
        Cards.remove_cards player.cards
          (Cards.pattern_troops player.cards)
      in
      place_stage_help3 pnum game player new_cards troops_left stage
  | "map" ->
      main game;
      place_stage game troops_left stage
  | _ ->
      print_string "\nYou done fucked up try again\n";
      place_stage game troops_left stage

and place_stage (game : gamestate) (troops_left : int) stage =
  let player = player_playing game in
  if winner player stage then place_stage_help5 player
  else if lost player then place_stage_help4 game stage
  else (
    if troops_left = 0 then battle_stage game false stage
    else place_stage_print game troops_left player;
    let pnum = game.state + 1 in
    if Cards.must_trade_in player.cards then
      placehelp6 game player pnum troops_left stage
    else if Cards.has_pattern player.cards then
      place_stage_help8 game player troops_left stage pnum
    else place_stage_help game player troops_left stage)

and place_stage_help8 game player troops_left stage pnum =
  print_string
    "\n\
     Enter which territory you want to place troops on or type trade \
     to exchange cards for troops";
  print_string "\n>";
  let x = read_line () in
  if List.mem x (name_list_owned player.terrs) then
    place_troops game x troops_left stage
  else if x = "quit" then exit 0
  else if x = "trade" then
    place_stage_help7 player game troops_left stage pnum
  else if x = "map" then (
    main game;
    place_stage game troops_left stage)
  else print_string "\nYou done fucked up try again\n";
  place_stage game troops_left stage

and place_stage_help7 player game troops_left stage pnum =
  let new_cards =
    Cards.remove_cards player.cards (Cards.pattern_troops player.cards)
  in
  place_stage_help2 pnum game troops_left player new_cards stage

and place_stage_help3 pnum game player new_cards troops_left stage =
  if pnum = 1 then
    place_stage
      { game with p1 = { player with cards = new_cards } }
      (troops_left + Cards.pattern_troops player.cards)
      stage
  else if pnum = 2 then
    place_stage
      { game with p2 = { player with cards = new_cards } }
      (troops_left + Cards.pattern_troops player.cards)
      stage
  else if pnum = 3 then
    place_stage
      { game with p3 = { player with cards = new_cards } }
      (troops_left + Cards.pattern_troops player.cards)
      stage
  else
    place_stage
      { game with p4 = { player with cards = new_cards } }
      (troops_left + Cards.pattern_troops player.cards)
      stage

and place_stage_help2 pnum game troops_left player new_cards stage =
  if pnum = 1 then
    place_stage
      { game with p1 = { player with cards = new_cards } }
      (troops_left + Cards.pattern_troops player.cards)
      stage
  else if pnum = 2 then
    place_stage
      { game with p2 = { player with cards = new_cards } }
      (troops_left + Cards.pattern_troops player.cards)
      stage
  else if pnum = 3 then
    place_stage
      { game with p3 = { player with cards = new_cards } }
      (troops_left + Cards.pattern_troops player.cards)
      stage
  else
    place_stage
      { game with p4 = { player with cards = new_cards } }
      (troops_left + Cards.pattern_troops player.cards)
      stage

and place_stage_help game player troops_left stage =
  print_string "\nEnter which territory you want to place troops on";
  print_string "\n>";
  let x = read_line () in
  if List.mem x (name_list_owned player.terrs) then
    place_troops game x troops_left stage
  else if x = "quit" then exit 0
  else if x = "map" then (
    main game;
    place_stage game troops_left stage)
  else print_string "\nYou done fucked up try again\n";
  place_stage game troops_left stage

and place_troops game terr (troops_left : int) stage =
  let player = player_playing game in
  print_string "\nEnter how many troops to place";
  print_string "\n>";
  let y = read_line () in
  let x = int_of_string y in
  if x >= 0 && x <= troops_left then
    place_troops_help2 game player terr stage x troops_left
  else if y = "map" then (
    main game;
    place_troops game terr troops_left stage)
  else print_string "\nYou done fucked up try again\n";
  place_troops game terr troops_left stage

and place_troops_help2 game player terr stage x troops_left =
  let pnum = game.state + 1 in
  if pnum = 1 then
    place_stage
      { game with p1 = plhelp player terr stage x }
      (troops_left - x) stage
  else if pnum = 2 then
    place_stage
      { game with p2 = plhelp player terr stage x }
      (troops_left - x) stage
  else if pnum = 3 then
    place_stage
      { game with p3 = plhelp player terr stage x }
      (troops_left - x) stage
  else
    place_stage
      { game with p4 = plhelp player terr stage x }
      (troops_left - x) stage

and plhelp player terr stage x =
  add_troops player (to_territory terr stage) x

let place_stage_init game stage =
  place_stage game (allocate_troops (player_playing game)) stage

let parse_players plays rolls terrs stage =
  place_stage_init
    (fully_init_game
       (List.hd (parse_names plays))
       (List.nth (parse_names plays) 1)
       (List.nth (parse_names plays) 2)
       (List.nth (parse_names plays) 3)
       rolls terrs)
    stage

let play_terrs y =
  ANSITerminal.print_string [ ANSITerminal.blue ] "\nRisk\n";
  print_string "Choose Map\n";
  print_string "Enter either 'full board' or 'test board' \n";
  print_string "> ";
  let x = read_line () in
  parse_players y
    (((Unix.gmtime (Unix.time ())).tm_sec + 1) * 10000)
    (if x = fake_board.name then fake_board.list else real_board.list)
    (if x = fake_board.name then fake_board.list else real_board.list)

let play () =
  ANSITerminal.print_string [ ANSITerminal.blue ] "\nRisk\n";
  print_string "Enter all 4 player names with spaces in between\n";
  print_string "> ";
  let x = read_line () in
  play_terrs x

let () = play ()
