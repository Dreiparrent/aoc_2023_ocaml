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

type char_type =
    | Number of listNumberChar
    | Symbol
    | Period
;;

let print_char_type ch_type =
    match ch_type with
    | Number n -> string_of_int n.testing_val
    (*
    | Number n ->
        let num = n.num in
        let num_val = !num.num_val in
        string_of_int !num_val
    *)
    | Symbol -> "S"
    | Period -> "P"

let _print_char_type_bool ch_type =
    match ch_type with
    | Number n ->
        let read = n.read in string_of_bool !read
    | Symbol -> "S"
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
    | i when i >= zero && i <= nine ->
            let current_val = char_int - zero in
            let new_number = match  prev_number with
                | Some prev_val ->
                    let num_ref = prev_val.num in
                    let num_val = !num_ref.num_val in
                    let () = num_val := !num_val * 10 + current_val in
                    let new_val = { prev_val with testing_val = current_val; point = pt } in
                    new_val
                | None -> {
                    num = ref {
                        num_val = ref current_val;
                    };
                    point = pt;
                    testing_val = current_val;
                    read = ref false
                }
            in Number new_number
    | 46 -> Period
    | _ -> Symbol
;;


let rec read_nodes_x x y prev_number acc number_acc str =
    if y < 0 || String.length str < y + 1 then
        (acc, number_acc)
    else
        let current_point = (x, y) in
        let char = str.[y] in
        let char_type = get_type char prev_number current_point in
        let (new_prev_number, new_num_list) = match char_type with
        | Number num_val ->
            (Some num_val, num_val :: number_acc)
        | _ -> (None, number_acc)
        in
        let new_acc = (char_type :: acc) in
        read_nodes_x x (y + 1) new_prev_number new_acc new_num_list str
;;

let rec read_all x acc number_acc str_list =
    match str_list with
    | [] -> (acc, number_acc)
    | hd :: tl ->
        let (char_list, num_list) = read_nodes_x x 0 None [] [] hd in
        let char_list_ordered = List.rev char_list in
        let num_list_ordered = List.rev num_list in
        let new_acc = char_list_ordered :: acc in
        let new_num_acc = num_list_ordered :: number_acc in
        read_all (x + 1) new_acc new_num_acc tl
;;


(* number_chars *)

let (full_map, number_chars) =
    let (current_map, num_map) = read_all 0 [] [] input_new_line_split in
    (List.rev current_map, List.rev num_map)

(* testing chars *)

(* testing *)
let rec _read_current lst =
    match lst with
    | [] -> ()
    | hd :: tl ->
        let () = List.iter (fun item -> print_string (print_char_type item)) hd in
        let () = print_newline () in
        _read_current tl
;;


let _read_current_numbers list =
    let rec print_numbers char_list =
        match char_list with
        | [] -> ()
        | hd :: tl ->
            let number_char = Number hd in
            let () = print_string ((print_char_type number_char) ^ " ") in
            print_numbers tl
    in
    let rec print_lines line_list =
        match line_list with
        | [] -> ()
        | hd :: tl ->
            let () = print_numbers hd in
            let () = print_newline () in
            print_lines tl
    in
    print_lines list
;;

(*
print_endline "full map";;
let () = read_current full_map;;
let () = read_current_numbers number_chars;;
let () = read_current printable_number;;
*)


(* here is the real deal *)
let points_list: point list =
    let poit_items = (-1, -1) :: (-1, 0) :: (-1, 1) :: (0, -1) :: (0, 1) :: (1, -1) :: (1, 0) :: (1, 1) :: [] in
    poit_items;;

let validate_pt_map (pt: point) =
    let (point_x, point_y) = pt in
    let rec check_all_points pts_list =
        match pts_list with
        | [] -> false
        | (x_offset, y_offset) :: tl ->
            let x = point_x + x_offset in
            let y = point_y + y_offset in
            (* let () = Printf.printf "check %i %i\n" x y in *)
            if x < 0 || y < 0 then
                check_all_points tl
            else
                let is_valid = match List.nth_opt full_map x with
                    | Some x_row ->
                        begin
                            match List.nth_opt x_row y with
                                | Some y_val ->
                                    y_val = Symbol
                                    (*
                                    begin
                                    match y_val with
                                    | Number _ -> true
                                    | _ -> false
                                    end
                                    *)
                                | None -> false
                        end
                    | None -> false
                in if is_valid then
                    true
                else
                    check_all_points tl
    in check_all_points points_list
;;


(*
let test_point = (0, 3);;
let test_point = (1, 0);;
let test_point = (5, 6);;
let () = print_newline ();;
let test_point = (1, 3);;

let () =
    let test_check = validate_pt_map  test_point in
    let checked = string_of_bool test_check in
    print_endline checked;;
*)



(* let get_nearbys acc pts_list = *)


let get_valid (pts: listNumberChar list list) =
    let rec row_is_valid acc row_pts =
        match row_pts with
        | [] -> acc
        | hd :: tl ->
            let pt = hd.point in
            let is_valid = validate_pt_map pt in
            let read = hd.read in
            let new_acc = match is_valid, !read with
            | true, false ->
                let () = read := true in
                hd :: acc
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

let valid_vins = get_valid number_chars;;

let rec get_final_ints acc (items: listNumberChar list) =
    match items with
    | [] -> acc
    | hd :: tl ->
        let num = hd.num in
        let num_val = !num.num_val in
        (*
        let () = print_int !num_val in
        let () = print_newline () in
        *)
        let new_acc = !num_val :: acc in
        get_final_ints new_acc tl
;;

let final_res = get_final_ints [] valid_vins;;

let final_ints = List.fold_left (+) 0 final_res;;

(* TODO: this is the problem*)
print_endline (string_of_int final_ints);;



