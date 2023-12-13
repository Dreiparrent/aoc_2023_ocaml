let input = "
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
";;

let is_part2 = false;;


module HandType : sig
    type t =
        | FiveOfKind
        | FourOfKind
        | FullHouse
        | ThreeOfKind
        | TwoPair
        | OnePair
        | High
        | Joker of int

    val enum_of_handType : t -> int
    val _handType_of_enum : int -> t
    val str_of_handType: t -> string
end = struct
    type t =
        | FiveOfKind
        | FourOfKind
        | FullHouse
        | ThreeOfKind
        | TwoPair
        | OnePair
        | High
        | Joker of int

    let enum_of_handType = function
        | FiveOfKind -> 7
        | FourOfKind -> 6
        | FullHouse -> 5
        | ThreeOfKind -> 4
        | TwoPair -> 3
        | OnePair -> 2
        | High -> 1
        | Joker _ -> 0 (* Joker always maps to 0 *)

    let _handType_of_enum = function
        | 7 -> FiveOfKind
        | 6 -> FourOfKind
        | 5 -> FullHouse
        | 4 -> ThreeOfKind
        | 3 -> TwoPair
        | 2 -> OnePair
        | 1 -> High
        | 0 -> Joker 0
        | _ -> invalid_arg "Invalid enum value for handType"

    let str_of_handType = function
        | FiveOfKind -> "FiveOfKind"
        | FourOfKind -> "FourOfKind"
        | FullHouse -> "FullHouse"
        | ThreeOfKind -> "ThreeOfKind"
        | TwoPair -> "TwoPair"
        | OnePair -> "OnePair"
        | High -> "High"
        | Joker _ -> "joker" (* failwith "should not print" *)
end


type game = {
    hand: char list;
    hand_map: (char, int) Hashtbl.t;
    hand_type: HandType.t;
    bid: int;
}

let _print_game gm =
    let hand_type_str = HandType.str_of_handType gm.hand_type in
    let hand_str = String.concat "" (List.map (String.make 1) gm.hand) in
    Printf.printf "hand = %s; hand_type = %s; bid = %i" hand_str hand_type_str gm.bid
;;

let rec _print_games list =
    match list with
    | [] -> ()
    | hd :: tl ->
        _print_game hd;
        print_newline ();
        _print_games tl
;;


let games =
    let input_split_line = String.trim input |> String.split_on_char '\n' in
    let input_split_space = List.map (String.split_on_char ' ') input_split_line in

    let rec get_hand table char_list =
        match char_list with
        | [] -> table
        | hd :: tl ->
            let exists = Hashtbl.mem table hd in
            if exists then
                let current_val = Hashtbl.find table hd in
                let () = Hashtbl.replace table hd (current_val + 1) in
                get_hand table tl
            else
                let () = Hashtbl.add table hd 1 in
                get_hand table tl
    in
    let get_basic_hand _ value acc =
        match value with
        | 5 -> HandType.FiveOfKind :: acc
        | 4 -> HandType.FourOfKind :: acc
        | 3 -> HandType.ThreeOfKind :: acc
        | 2 -> HandType.OnePair :: acc
        | _ -> HandType.High :: acc
    in

    let get_joker_hand hand =
        let get_joker_vals key value (max_key, max_val, j, has_non_j) =
            if key = 'J' then
                (max_key, max_val, value, has_non_j)
            else
                if value > max_val then
                    (key, value, j, true)
                else
                    (max_key, max_val, j, true)
        in
        let (highest_key, highest_val, jokers, has_non_j) = Hashtbl.fold get_joker_vals  hand ('J', min_int, 0, false) in
        if has_non_j then
            let () = Hashtbl.replace hand highest_key (highest_val + jokers) in
            let () = Hashtbl.remove hand 'J' in
            hand
        else
            hand
    in

    let get_hand_calculated (hand: (char, int) Hashtbl.t) =
        let joker_hand = match is_part2 with
        | true -> get_joker_hand hand
        | false -> hand
        in
        let hand_parsed = Hashtbl.fold get_basic_hand joker_hand [] in
        let current_hand_val = match hand_parsed with
            | [HandType.FiveOfKind] -> HandType.FiveOfKind
            | [HandType.FourOfKind; HandType.High] -> HandType.FourOfKind
            | [HandType.High; HandType.FourOfKind] -> HandType.FourOfKind
            | [HandType.ThreeOfKind; HandType.OnePair] -> HandType.FullHouse
            | [HandType.OnePair; HandType.ThreeOfKind] -> HandType.FullHouse
            | [HandType.ThreeOfKind; HandType.High; HandType.High] -> HandType.ThreeOfKind
            | [HandType.High; HandType.ThreeOfKind; HandType.High] -> HandType.ThreeOfKind
            | [HandType.High; HandType.High; HandType.ThreeOfKind] -> HandType.ThreeOfKind
            | item ->
                if List.length item = 3 then
                    HandType.TwoPair
                else if List.mem HandType.OnePair item then
                    HandType.OnePair
                else
                    HandType.High
        in
        current_hand_val
    in
    let rec get_final_games acc split_spc =
        let hand_map = Hashtbl.create 16 in
        match split_spc with
        | [] -> acc
        | [str_hand; bid] :: tl ->
            let hand = str_hand |> String.to_seq |> List.of_seq in
            let hand_map = get_hand hand_map hand in
            let hand_type = get_hand_calculated hand_map in
            let gm = {
                hand;
                hand_map = hand_map;
                hand_type = hand_type;
                bid = int_of_string bid
            } in
            let new_acc = gm :: acc in
            get_final_games new_acc tl
        | _ -> failwith "this is impossible"
    in
    List.rev (get_final_games [] input_split_space)
;;

(* _print_games games;; *)

let card_char_ordered =
    let part_card_order = match is_part2 with
    | true -> "A K Q T 9 8 7 6 5 4 3 2 J"
    | false -> "A K Q J T 9 8 7 6 5 4 3 2"
    in
    part_card_order
        |> String.split_on_char ' '
        |> String.concat ""
        |> String.to_seq
        |> List.of_seq
        |> List.rev
;;

let sort_hands (game_hands: game list) =
    let get_card_value card_char =
        let rec get_card_inner val_list idx =
            match val_list with
            | [] -> failwith "invalid card supplied"
            | hd :: tl ->
                if hd = card_char then
                    idx
                else
                    get_card_inner tl (idx + 1)
        in
        get_card_inner card_char_ordered 0
    in

    let card_sorter game1 game2 =
        let rec compare_cards chars1 chars2 =
            match chars1, chars2 with
            | [], [] -> 0
            | [], _ -> -1
            | _, [] -> 1
            | hd1 :: tl1, hd2 :: tl2 ->
                let card_comparison = compare (get_card_value hd1) (get_card_value hd2) in
                if card_comparison = 0 then
                    compare_cards tl1 tl2
                else
                    card_comparison
        in
        let compare_hands g1 g2 =
            let hand_comparison = compare (HandType.enum_of_handType g1.hand_type) (HandType.enum_of_handType g2.hand_type) in
            if hand_comparison <> 0 then
                hand_comparison
            else
                compare_cards g1.hand g2.hand

        in
        compare_hands game1 game2
    in
    List.sort card_sorter game_hands
;;

let sorted_hands = sort_hands games;;
_print_games sorted_hands;;

let total_value = List.mapi (fun i game -> game.bid * (i + 1)) sorted_hands |> List.fold_left (+) 0;;

Printf.printf "total = %i\n" total_value;;
