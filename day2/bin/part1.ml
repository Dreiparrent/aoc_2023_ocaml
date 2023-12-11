let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green";;

let games_new_line_split = String.split_on_char '\n' input;;


type ball =
    | Red of int
    | Blue of int
    | Green of int
;;

type ballCounter = {
    red: int;
    green: int;
    blue: int;
}

let get_ball color ball_count =
    match color with
    | "red" -> Red ball_count
    | "blue" -> Blue ball_count
    | "green" -> Green ball_count
    | _ -> failwith "Invalid color string"




let rec game_items acc games_str =
    match games_str with
    | [] -> acc
    | hd :: tl ->
        let game_split = String.split_on_char ':' hd in
        let new_acc = String.trim (List.nth game_split 1) :: acc in
        game_items new_acc tl
;;


let rec get_subgames acc games_str =
    match games_str with
    | [] -> acc
    | hd :: tl ->
        let game_split = String.split_on_char ';' hd in
        let new_acc = game_split :: acc in
        get_subgames new_acc tl
;;


let rec get_game_balls_str acc sub_game =
    match sub_game with
    | [] -> acc
    | hd :: tl ->
        let ball_split = String.split_on_char ',' hd in
        let new_acc = ball_split :: acc in
        get_game_balls_str new_acc tl
;;


let rec get_ball_value acc game_ball =
    match game_ball with
    | [] -> acc
    | hd :: tl ->
        let ball_str = String.trim hd in
        let ball_str_split = String.split_on_char ' ' ball_str in
        let ball_count = int_of_string (List.hd ball_str_split) in
        let color = List.nth ball_str_split 1 in
        let new_acc = get_ball color ball_count :: acc in
        get_ball_value new_acc tl
;;

let rec get_subgame_balls acc subgame_str =
    match subgame_str with
    | [] -> acc
    | hd :: tl ->
        let game_balls = get_ball_value [] hd in
        let new_acc = game_balls :: acc in
        get_subgame_balls new_acc tl

let rec get_game_balls acc subgames_split =
    match subgames_split with
    | [] -> acc
    | hd :: tl ->
        let subgame_balls_str = get_game_balls_str [] hd in
        let balls = get_subgame_balls [] subgame_balls_str in
        let new_acc = balls :: acc in
        get_game_balls new_acc tl



let rec check_your_balls game_balls valid_game =
    match game_balls with
    | [] -> true
    | hd :: tl ->
        let is_valid = match hd with
        | Red ball -> valid_game.red >= ball
        | Blue ball -> valid_game.blue >= ball
        | Green ball -> valid_game.green >= ball
        in if is_valid then
            check_your_balls tl valid_game
        else
            false
;;

let rec validate_subgame acc subgame_balls valid_game =
    match subgame_balls with
    | [] -> acc
    | hd :: tl ->
        let is_valid_subgame = check_your_balls hd valid_game in
        let new_acc = is_valid_subgame :: acc in
        validate_subgame new_acc tl valid_game
;;

let rec get_valid_games acc games_balls valid_game =
    match games_balls with
    | [] -> acc
    | hd :: tl ->
        let valid_sub = validate_subgame [] hd valid_game in
        let new_acc = valid_sub :: acc in
        get_valid_games new_acc tl valid_game
;;

let rec validate_game subgame_validity =
    match subgame_validity with
    | [] -> true
    | hd :: tl ->
        if hd = true then
            validate_game tl
        else
            false
;;


let game_setup = {
    red = 12;
    green = 13;
    blue = 14;
};;

let games =
    let games_cleaned = game_items [] games_new_line_split in
    let subgames_split = get_subgames [] games_cleaned in
    get_game_balls [] subgames_split
;;

let subgames_validated = get_valid_games [] games game_setup;;

let rec is_valid_games i acc game_balls =
    match game_balls with
    | [] -> acc
    | hd :: tl ->
        let game_valid = validate_game hd in
        let new_acc = match game_valid with
        | true -> i :: acc
        | false -> acc
        in is_valid_games (i + 1) new_acc tl
;;

let valid_games = is_valid_games 1 []  subgames_validated;;

let rec print_valid list =
    match list with
    | [] -> ()
    | hd :: tl ->
        let () = print_int hd in
        let () = print_newline () in
        print_valid tl
;;

let () = print_valid valid_games;;

let sum_list list =
  List.fold_left (fun acc x -> acc + x) 0 list;;

let final_output = sum_list valid_games;;

let () = print_int final_output;;
let () = print_newline ();;
