let input = "
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4

asdf:
"

let _print_maps maps_list =
    let rec do_print list =
        match list with
        | [] -> ()
        | hd :: tl ->
            let () = print_endline hd in
            do_print tl
    in
    let rec print_outer list=
        match list with
        | [] -> ()
        | hd :: tl ->
            let () = do_print hd in
            let () = print_newline () in
            print_outer tl
    in
    print_outer maps_list
;;

let maps_str =
    let get_maps split_str =
        let rec get_maps_inner acc inner_acc str_list =
            match str_list with
            | [] -> acc
            | hd :: tl ->
                if String.length hd < 1 then
                    let new_acc = (List.rev inner_acc) :: acc in
                    get_maps_inner new_acc [] tl
                else
                    let new_inner_acc = hd :: inner_acc in
                    get_maps_inner acc new_inner_acc tl

        in
        List.rev (get_maps_inner [] [] split_str)
    in
    let input_split_line = String.split_on_char '\n' (String.trim input) in
    get_maps input_split_line
    (* get_maps_inner [] [] split_str *)
;;

type mapping = {
    out_val: int;
    in_val: int;
    range: int;
}

let _print_mapping mapping =
    Printf.printf "%i %i %i" mapping.out_val mapping.in_val mapping.range

let read_maps map_list =
    let rec read_map_inner acc list =
        match list with
        | [] -> acc
        | hd :: tl ->
            let new_acc =
                begin
                match List.map int_of_string (String.split_on_char ' ' hd) with
                | [out_val; in_val; range] -> { out_val; in_val; range } :: acc
                | _ -> failwith "there are always three items"
                end in
            read_map_inner new_acc tl
    in
    let rec read_map_outer acc list =
        match list with
        | [] -> acc
        | hd :: tl ->
            let map_vals = List.rev (read_map_inner [] (List.tl hd)) in
            let new_acc = map_vals :: acc in
            read_map_outer new_acc tl
    in
    let seed_list = String.split_on_char ' '
        (
            String.trim (
                List.hd (
                    List.rev (
                        String.split_on_char ':' (
                            String.trim (List.hd (List.hd map_list))
                        )
                    )
                )
            )
        )
    in

    List.map (int_of_string) seed_list, List.rev (read_map_outer [] (List.tl map_list))
;;

let (seeds, mappings) = read_maps maps_str;;

(*
let _print_final_list map_list =
    List.iter (function i ->
        let () = List.iter (function j ->
            let () = _print_mapping j in
            print_newline ()
        ) i in
        print_newline ()
    ) map_list
;;
let () = _print_final_list mappings;;
List.iter (function i -> print_endline (string_of_int i)) seeds;;
*)



let get_location_for_seed seed =
    let rec check_range inner_maps inner_seed =
        match inner_maps with
        | [] -> inner_seed
        | hd :: tl ->
            let lower = hd.in_val in
            let upper = lower + hd.range in
            if inner_seed >= lower && inner_seed < upper then
                let offset = hd.out_val - hd.in_val in
                inner_seed + offset
            else
                check_range tl inner_seed
    in
    let rec check_all_maps map_list inner_seed =
        match map_list with
        | [] -> inner_seed
        | hd :: tl ->
            let new_seed = check_range hd inner_seed in
            (*let () = Printf.printf "new seed = %i\n" new_seed in *)
            check_all_maps tl new_seed
    in
    check_all_maps mappings seed


let rec get_all_locations lowest_location list =
    match list with
    | [] -> lowest_location
    | hd :: tl ->

        let new_seed = get_location_for_seed hd in
        (* Printf.printf "in = %i, out = %i\n" hd new_seed; *)
        if new_seed < lowest_location then
            get_all_locations new_seed tl
        else
            get_all_locations lowest_location tl
;;

let lowest = get_all_locations max_int seeds;;
Printf.printf "lowest = %i\n" lowest;;

(* this is a really bad part2 response *)

let rec get_all_locations_int lowest_location start stop =
    if start < stop then
        let new_seed = get_location_for_seed start in
        (* Printf.printf "in = %i, out = %i\n" hd new_seed; *)
        if new_seed < lowest_location then
            get_all_locations_int new_seed (start + 1) stop
        else
            get_all_locations_int lowest_location (start + 1) stop
    else
        lowest_location

;;

let rec _test_all lowest_location list =
    match list with
    | [] -> lowest_location
    | start :: range :: rest ->
        print_endline ("basic: " ^ (string_of_int start) ^ "\n");
        (*
        let sub_list = List.init range (fun i -> start + i - 1) in
        let new_lowest = get_all_locations lowest_location sub_list in
        *)
        let new_lowest = get_all_locations_int lowest_location start (start + range - 1) in
        if new_lowest < lowest_location then
            _test_all new_lowest rest
        else
            _test_all lowest_location rest
    | _ -> failwith "The list should have an even number of elements"
;;


(*
let lowest = _test_all max_int seeds;;
Printf.printf "lowest = %i\n" lowest;;

TODO: what is needed for part 2,
which I actually thought about before part 1
(but decided it would be harder than brute-force)
is to start at the list of locations,
then generate all ranges of inputs,
then use this in a btree to check the inputs
*)
