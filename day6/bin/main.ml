let input_str = "
Time:      7  15   30
Distance:  9  40  200
";;

type raceResult = {
    time: int;
    distance: int;
}

let mapped_input_part1 colon_split =
    let rec create_results i acc times distances =
        if i < 0 then
            acc
        else
            let res = { time = List.nth times i; distance = List.nth distances i} in
            create_results (i - 1) (res :: acc) times distances
    in
    let rec get_final_nums_part1_inner acc str_list =
        match str_list with
        | [] ->  acc
        | hd :: tl ->
            let real_hd = String.trim hd in
            let str_len = String.length real_hd in
            if str_len > 0 then
                let int_val = int_of_string real_hd in
                let new_acc = int_val :: acc in
                get_final_nums_part1_inner new_acc tl
            else
                get_final_nums_part1_inner acc tl
    in
    let times = get_final_nums_part1_inner [] (List.rev (String.split_on_char ' ' (List.hd colon_split))) in
    let distances = get_final_nums_part1_inner [] ( List.rev (String.split_on_char ' ' (List.nth colon_split 1))) in
    create_results ((List.length times) - 1) [] times distances
;;


let mapped_input_part2 colon_split =
    let rec get_final_nums_part2 acc digits str_list =
        match str_list with
        | [] ->  acc
        | hd :: tl ->
            let real_hd = String.trim hd in
            let str_len = String.length real_hd in
            if str_len > 0 then
                let int_val = int_of_string real_hd in
                let e = 10. ** digits in
                let new_acc = float_of_int int_val *. e +. acc in
                let new_digits = float_of_int str_len +. digits in
                get_final_nums_part2 new_acc new_digits tl
            else
                get_final_nums_part2 acc digits tl
    in
    let time = get_final_nums_part2 0. 0. (List.rev (String.split_on_char ' ' (List.hd colon_split))) in
    let distance = get_final_nums_part2 0. 0. ( List.rev (String.split_on_char ' ' (List.nth colon_split 1))) in
    { time = int_of_float time; distance = int_of_float distance } :: []
;;


let mapped_input is_part1 =
    let input_split_line = String.split_on_char '\n' (String.trim input_str) in
    let colon_split = List.map (fun i -> List.nth (String.split_on_char ':' i) 1) input_split_line in
    if is_part1 then
        mapped_input_part1 colon_split
    else
        mapped_input_part2 colon_split
;;

(* lol I started here *)
let f (x, total) = x * (total -x);;

let find_all_winners input_maps =
    let rec find_winners acc current_time max_time distance_to_beat =
        if current_time = max_time then
            acc
        else
            let distance = f (current_time, max_time) in
            let new_acc = match  distance > distance_to_beat with
            | true -> 1 + acc
            | false -> acc
            in
            find_winners new_acc (current_time + 1) max_time distance_to_beat

    in
    let rec sum_winners acc list =
        match list with
        | [] -> acc
        | { time; distance } :: tl ->
            let winners = find_winners 0 0 time distance in
            sum_winners (winners * acc) tl
    in
    sum_winners 1 input_maps
;;

let res_part1 = find_all_winners (mapped_input true);;
let res_part2 = find_all_winners (mapped_input false);;

let () = print_endline ("final out part1: " ^ (string_of_int (res_part1)));;
let () = print_endline ("final out part2: " ^ (string_of_int (res_part2)));;
