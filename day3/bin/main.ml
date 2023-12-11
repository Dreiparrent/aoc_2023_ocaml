let input = "
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
";;
let input_new_line_split = String.split_on_char '\n' (String.trim input);;

let zero = Char.code '0';;
let nine = Char.code '9';;
let is_char i = i >= zero && i <= nine;;

type point = int * int;;

type numberChar = {
    num_val: int ref;
}

type listNumberChar = {
    num: numberChar ref;
    point: point;
    testing_val: int;
    read: bool ref;
}

type listSymbolChar = {
    is_star: bool;
    point: point;
}

type char_type =
    | Number of listNumberChar
    | Symbol of listSymbolChar
    | Period
;;

let _print_char_type ch_type =
    match ch_type with
    | Number n -> string_of_int n.testing_val
    | Symbol _ -> "S"
    | Period -> "P"

let _print_char_type_bool ch_type =
    match ch_type with
    | Number n ->
        let read = n.read in string_of_bool !read
    | Symbol _ -> "S"
    | Period -> "P"


let rec _print_points pt =
    let print_point (x, y) = Printf.printf "(%d, %d)\n" x y in
    match pt with
    | [] -> ()
    | hd :: tl ->
        let () = print_point hd in
        _print_points tl
;;


let get_type char prev_number pt =
    let char_int = Char.code char in
    match char_int with
    | i when is_char i ->
        let current_val = char_int - zero in
        let new_number = match  prev_number with
        | Some prev_val ->
            let num_ref = prev_val.num in
            let num_val = !num_ref.num_val in
            let () = num_val := !num_val * 10 + current_val in
            let new_val = { prev_val with testing_val = current_val; point = pt } in
            new_val
        | None ->
            {
                num = ref {
                    num_val = ref current_val;
                };
                point = pt;
                testing_val = current_val;
                read = ref false
            }
        in
        Number new_number
    | 46 -> Period
    | 42 -> let symbolType = { is_star = true; point = pt } in Symbol symbolType
    | _ -> let symbolType = { is_star = false; point = pt } in Symbol symbolType
;;


let rec read_nodes_x x y prev_number acc number_acc symbol_acc str =
    if y < 0 || String.length str < y + 1 then
        (acc, number_acc, symbol_acc)
    else
        let current_point = (x, y) in
        let char = str.[y] in
        let char_type = get_type char prev_number current_point in
        let (new_prev_number, new_num_list, new_symbol_list) = match char_type with
        | Number num_val ->
            (Some num_val, num_val :: number_acc, symbol_acc)
        | Symbol { is_star = true; point } ->
            (None, number_acc, point :: symbol_acc)
        | _ ->
            (None, number_acc, symbol_acc)
        in
        let new_acc = (char_type :: acc) in
        read_nodes_x x (y + 1) new_prev_number new_acc new_num_list new_symbol_list str
;;


let rec read_all x acc number_acc symbol_acc str_list =
    match str_list with
    | [] -> (acc, number_acc, symbol_acc)
    | hd :: tl ->
        let (char_list, num_list, symbol_list) = read_nodes_x x 0 None [] [] [] hd in

        let char_list_ordered = List.rev char_list in
        let num_list_ordered = List.rev num_list in
        let symbol_list_ordered = List.rev symbol_list in

        let new_acc = char_list_ordered :: acc in
        let new_num_acc = num_list_ordered :: number_acc in
        let new_symbol_list = symbol_list_ordered :: symbol_acc in

        read_all (x + 1) new_acc new_num_acc new_symbol_list tl
;;


(* number_chars *)

let (full_map, _number_points, symbol_points) =
    let (current_map, num_map, symbol_map) = read_all 0 [] [] [] input_new_line_split in
    (List.rev current_map, List.rev num_map, List.rev symbol_map)


(* here is the real deal *)
let points_list: point list = (-1, -1) :: (-1, 0) :: (-1, 1) :: (0, -1) :: (0, 1) :: (1, -1) :: (1, 0) :: (1, 1) :: [];;


let validate_pt_map (pt: point) =
    let (point_x, point_y) = pt in
    let rec check_all_points acc pts_list =
        match pts_list with
        | [] -> acc
        | (x_offset, y_offset) :: tl ->
            let x = point_x + x_offset in
            let y = point_y + y_offset in
            if x < 0 || y < 0 then
                check_all_points acc tl
            else
                let new_acc = match List.nth_opt full_map x with
                | Some x_row ->
                    begin match List.nth_opt x_row y with
                    | Some Number number_ch ->
                        let read = number_ch.read in
                        let num = number_ch.num in
                        let num_val = !num.num_val in
                        if !read then
                            acc
                        else
                            let () = read := true in
                            !num_val :: acc
                    | _ -> acc
                    end
                | None -> acc
                in
                check_all_points new_acc tl
    in check_all_points [] points_list
;;


(*
let test_point = (1, 3);;
let () =
    let rec print_all list =
        match list with
        | [] -> ()
        | hd :: tl ->
            let () = print_int hd in
            let () = print_char ' ' in
            print_all tl
    in
    let test_check = validate_pt_map test_point in
    print_all test_check
;;
let test_point = (8, 5);;
let test_point = (4, 3);;

*)


(* let get_nearbys acc pts_list = *)

let get_valid (pts: point list list) =
    let rec row_is_valid acc row_pts =
        match row_pts with
        | [] -> acc
        | hd :: tl ->
            let valid_list = validate_pt_map hd in
            let valid_len = List.length valid_list in
            let (new_acc) = match valid_len, valid_list with
            | 2, valid_vals ->
                let product = List.fold_left ( * ) 1 valid_vals in
                product :: acc
            | _ -> acc
            in row_is_valid new_acc tl
    in
    let rec validate_rows acc game =
        match game with
        | [] -> acc
        | hd :: tl ->
            let is_valid = row_is_valid [] hd in
            let new_is_valid = is_valid @ acc in
            validate_rows new_is_valid tl
    in
    let validated = validate_rows [] pts in
    validated
;;


let final_value =
    let valid_gears = get_valid symbol_points in
    List.fold_left (+) 0 valid_gears;;

print_endline (string_of_int final_value);;
