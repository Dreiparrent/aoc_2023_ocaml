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
    let col_split = split_on_colon [] input_split_line in
    List.rev col_split


type gameItems = {
    index: int;
    winning_nums: int list;
    chosen_nums: int list;
    mutable winners: (int * gameItems list) option;
}

let _print_game_item game_item =
    Printf.printf "index %i" game_item.index;
    print_newline ()
;;

let get_games game_str =
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
    let rec get_strs_for_game acc i str_list =
        match str_list with
        | [] -> acc
        | hd :: tl ->
            (* let () = print_endline hd in *)
            let (winners, chosen) = split_game_and_number hd in
            let winner_vals = get_numbers [] (String.split_on_char ' ' winners) in
            let chosen_vals = get_numbers [] (String.split_on_char ' ' chosen) in
            let game_item = { index = i; winning_nums = winner_vals; chosen_nums = chosen_vals; winners = None } in
            let new_acc = game_item :: acc in
            get_strs_for_game new_acc (i + 1) tl
    in
    let games = get_strs_for_game [] 0 game_str in
    List.rev games
;;

let split_game = get_games split_all_on_colon;;


(* the solution part *)

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
    let rec get_game_wins acc games =
        if Queue.is_empty games then
            acc
        else
            (* TODO: make this memory efficient by passing a ref instead of a new item
               i.e. we are creating a copy of head in add_items
            *)
            let hd = Queue.take games in
            let {index; winning_nums; chosen_nums; winners} = hd in
            let (count_winning, add_items) = match winners with
            | None ->
                let winning_items = get_winner_for_game chosen_nums [] winning_nums in
                let ct = List.length winning_items in
                let item_indices = List.init (ct) (fun i -> index + i + 1) in
                let add_items =
                    List.map (fun i -> List.nth_opt games_list i) item_indices
                    |> List.filter (fun item -> Option.is_some item)
                    |> List.map (function
                        | Some x -> x
                        | None -> failwith "Unexpected None"
                    )
                in
                let res = (ct, add_items) in
                let () = hd.winners <- Some res in
                res
            | Some wins -> wins
            in
            let () = List.iter (fun item -> Queue.add item games) add_items  in
            let new_game_winners = count_winning + acc in
            get_game_wins new_game_winners games
    in
    let games_queue =
        let queue = Queue.create ()
        in
        let rec enqueue_games games =
            match games with
            | [] -> queue
            | hd :: tl ->
                (* let () = _print_game_item hd in *)
                let () = Queue.add hd queue in
                enqueue_games tl
        in
        enqueue_games games_list
    in
    get_game_wins (List.length games_list) games_queue

let game_winners = get_winners split_game;;

print_endline (string_of_int game_winners);;
