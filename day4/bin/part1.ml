let input = "
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
";;


let input_split_line = String.split_on_char '\n' (String.trim input);;

let split_all_on_colon =
    let rec split_on_colon acc str =
        match str with
        | [] -> acc
        | hd :: tl ->
            let split_col = String.split_on_char ':' hd in
            let wanted_item = List.nth split_col 1 in
            let new_acc = wanted_item :: acc in
            split_on_colon new_acc tl
    in
    split_on_colon [] input_split_line



let get_games_str game_str =
    let rec get_numbers acc split_by_space =
        match split_by_space with
        | [] -> acc
        | hd :: tl ->
            if String.length hd < 1 then
                get_numbers acc tl
            else
                let inv_val = int_of_string (String.trim hd) in
                let new_acc = inv_val :: acc in
                get_numbers new_acc tl
    in
    let split_game_and_number game_str =
        let split_res = String.split_on_char '|' (String.trim game_str) in
        (List.hd split_res, List.nth split_res 1)
    in
    let rec get_strs_for_game acc str_list =
        match str_list with
        | [] -> acc
        | hd :: tl ->
            let (winners, chosen) = split_game_and_number hd in
            let winner_vals = get_numbers [] (String.split_on_char ' ' winners) in
            let chosen_vals = get_numbers [] (String.split_on_char ' ' chosen) in
            let new_acc = (winner_vals, chosen_vals) :: acc in
            get_strs_for_game new_acc tl
    in
    get_strs_for_game [] game_str
;;

let split_game = get_games_str split_all_on_colon;;


let get_winners games_list =
    let rec get_winner_for_game chosen winners winning_vals =
        match chosen with
        | [] -> winners
        | hd :: tl ->
            if List.mem hd winning_vals then
                let new_winners = hd :: winners in
                get_winner_for_game tl new_winners winning_vals
            else
            get_winner_for_game tl winners winning_vals
    in
    let rec get_game_wins game_winners games =
        match games with
        | [] -> game_winners
        | hd :: tl ->
                let (winnings_vals, chosen_vals) = hd in
                let winning_items  = get_winner_for_game chosen_vals [] winnings_vals in
                let new_game_winners = winning_items :: game_winners in
                get_game_wins new_game_winners tl
    in
    get_game_wins [] games_list

let game_winners = get_winners split_game;;

let print_ints int_list =
    let rec print_int int =
        match int with
        | [] -> ()
        | hd :: tl ->
            let str_int = string_of_int hd in
            let () = print_string (str_int ^ " ") in
            print_int tl
    in
    let rec print_all list =
        match list with
        | [] -> ()
        | hd :: tl ->
            let () = print_int hd in
            let () = print_newline () in
            print_all tl
    in
    print_all int_list
;;

print_ints game_winners;;


let get_total int_list =
    let get_win_val list =
        let pow_val = (List.length list) - 1 in
        if pow_val < 0 then
            0.
        else
            2. ** float_of_int pow_val
    in
    let rec get_game_val acc list =
        match list with
        | [] -> acc
        | hd :: tl ->
            let game_val = get_win_val hd in
            get_game_val (acc +. game_val) tl
    in
    get_game_val 0. int_list
;;

print_endline "final";;

let total_res = get_total game_winners;;
print_endline (string_of_float total_res)
