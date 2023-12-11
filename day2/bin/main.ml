let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green";;

let games_new_line_split = String.split_on_char '\n' input;;


(* ball definitions *)
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
;;


let rec get_game_str acc games_str =
    match games_str with
    | [] -> acc
    | hd :: tl ->
        let game_split = String.split_on_char ':' hd in
        let new_acc = String.trim (List.nth game_split 1) :: acc in
        get_game_str new_acc tl
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


let game_setup = {
    red = 0;
    green = 0;
    blue = 0;
};;

let games =
    let game_str = get_game_str [] games_new_line_split in
    let subgames_split = get_subgames [] game_str in
    get_game_balls [] subgames_split
;;


let rec check_your_balls game_balls current_max =
    match game_balls with
    | [] -> current_max
    | hd :: tl ->
        let new_current_max = match hd with
        | Red ball_count when ball_count > current_max.red -> { current_max with red = ball_count }
        | Blue ball_count when ball_count > current_max.blue -> { current_max with blue = ball_count }
        | Green ball_count when ball_count > current_max.green -> { current_max with green = ball_count }
        | _ -> current_max
        in check_your_balls tl new_current_max
;;

let rec get_least_in_subgame game_balls current_max =
    match game_balls with
    | [] -> current_max
    | hd :: tl ->
        let new_current_max = check_your_balls hd current_max in
        get_least_in_subgame tl new_current_max
;;

let rec get_least_game_balls acc games_balls current_max =
    match games_balls with
    | [] -> acc
    | hd :: tl ->
        let least_possible_sub = get_least_in_subgame hd current_max in
        let new_acc = least_possible_sub :: acc in
        get_least_game_balls new_acc tl current_max
;;

let games_least_balls = get_least_game_balls [] games game_setup;;

let rec get_games_powers acc games =
    match games with
    | [] -> acc
    | hd :: tl ->
        let power = hd.red * hd.blue * hd.green in
        let new_acc = power :: acc in
        get_games_powers new_acc tl
;;


let final_res =
    let game_powers = get_games_powers [] games_least_balls in
    List.fold_left (+) 0 game_powers
;;

print_int final_res;;
print_newline ();;
