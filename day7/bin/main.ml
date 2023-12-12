let input = "
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
";;


type handType =
    | FiveOfKind
    | FourOfKind
    | FullHouse
    | ThreeOfKind
    | TwoPair
    | OnePair
    | High
;;


type game = {
    hand: char list;
    hand_map: (char, int) Hashtbl.t;
    hand_type: handType;
    bid: int;
}

let _print_game gm =
    let get_hand_type hand_type =
        match hand_type with
        | FiveOfKind -> "FiveOfKind"
        | FourOfKind -> "FourOfKind"
        | FullHouse -> "FullHouse"
        | ThreeOfKind -> "ThreeOfKind"
        | TwoPair -> "TwoPair"
        | OnePair -> "OnePair"
        | High -> "High"
    in
    let hand_str = String.concat "" (List.map (String.make 1) gm.hand) in
    Printf.printf "hand = %s; hand_type = %s; bid = %i" hand_str (get_hand_type gm.hand_type) gm.bid

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
    let get_basic_hand key value acc =
        match key, value with
        | _, 5 -> FiveOfKind :: acc
        | _, 4 -> FourOfKind :: acc
        | _, 3 -> ThreeOfKind :: acc
        | _, 2 -> OnePair :: acc
        | _ -> High :: acc
    in
    let get_hand_calculated hand =
        let hand_parsed = Hashtbl.fold get_basic_hand hand [] in
        match hand_parsed with
        | [FiveOfKind] -> FiveOfKind
        | [FourOfKind; High] -> FourOfKind
        | [High; FourOfKind] -> FourOfKind
        | [ThreeOfKind; OnePair] -> FullHouse
        | [OnePair; ThreeOfKind] -> FullHouse
        | [ThreeOfKind; High; High] -> ThreeOfKind
        | [High; ThreeOfKind; High] -> ThreeOfKind
        | [High; High; ThreeOfKind] -> ThreeOfKind
        | item ->
            if List.length item = 3 then
                TwoPair
            else if List.mem OnePair item then
                OnePair
            else
                High
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

let _print_key_value key value =
  Printf.printf "Key: %c, Value: %d\n" key value
;;


(* _print_games games;; *)


let card_char_ordered =
    "A K Q J T 9 8 7 6 5 4 3 2"
        |> String.split_on_char ' '
        |> String.concat ""
        |> String.to_seq
        |> List.of_seq
        |> List.rev

let sort_hands (game_hands: game list) =
    let get_hand_value hand_type =
        match hand_type with
        | FiveOfKind -> 6
        | FourOfKind -> 5
        | FullHouse -> 4
        | ThreeOfKind -> 3
        | TwoPair -> 2
        | OnePair -> 1
        | High -> 0
    in
    let get_card_value card_char =
        let rec get_card_inner val_list idx =
            match val_list with
            | [] -> failwith "impossible"
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
            let hand_comparison = compare (get_hand_value g1.hand_type) (get_hand_value g2.hand_type) in
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
