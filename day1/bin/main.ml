let input = "
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
";;


let input_new_line_split = String.split_on_char '\n' input

let zero = Char.code '0';;
let nine = Char.code '9';;


let rec get_char_forward str i =
  if i < String.length str then
    let char_code = Char.code str.[i] in
    if char_code >= zero && char_code <= nine then
      char_code - zero
    else
      get_char_forward str (i + 1)
  else
    0

let rec get_char_reversed str i =
  if i >= 0 && i < String.length str then
    let char_code = Char.code str.[i] in
    if char_code >= zero && char_code <= nine then
      char_code - zero
    else
      get_char_reversed str (i - 1)
  else
    0

let forward =
  let read_per_line elem = get_char_forward elem 0 in
  List.map read_per_line input_new_line_split

let reversed =
  let read_per_line elem =
  let elem_len = String.length elem - 1 in
  get_char_reversed elem elem_len in
  List.map read_per_line input_new_line_split

(* let forward = forward @ reversed;; *)


let rec sum_list index acc forward_list reverse_list =
  match forward_list with
  | [] -> acc
  | head :: tail -> let ones_digit = List.nth reversed index in
    let new_acc = head * 10 + ones_digit + acc in
    sum_list (index + 1) new_acc tail reverse_list


let part_one_answer = sum_list 0 0 forward reversed;;

print_int part_one_answer;;
print_endline ""


(* this is the part two answer *)


(* helpers *)

let rec read_char_list list =
  match list with
  | [] -> ()
  | hd :: tl ->
      let real_char = Char.chr hd in
      let () = print_char real_char in
      read_char_list tl
      (*
      let () = print_int hd in
      *)


let rec read_char_lines list =
  match list with
  | [] -> ()
  | hd :: tl ->
      let () = read_char_list hd in
      let () = print_newline () in
      read_char_lines tl



let range a b =
  if a > b then []
  else List.init (b - a + 1) (fun i -> a + i)
;;

let list_of_possible = range zero nine;;

let rec generate_char_list_for_str_base i acc str =
  if i >= 0 && i < String.length str then
    let current_char = str.[i] in
    let char_code = Char.code current_char in
    let new_acc = char_code :: acc in
    generate_char_list_for_str_base (i - 1) new_acc str
  else
    acc

let generate_char_list_for_str str = generate_char_list_for_str_base (String.length str - 1) [] str

type asdf = {
  real_val: int;
  str_val: char list;
}

let test_str_list = [{ real_val =  9; str_val = [ '9' ] }];;
let simplified_test = List.hd test_str_list;;

let () = print_endline  "string here";;

let rec check_based_on_list i acc str =
  if i >= 0 && i < String.length str then
    let current_char = str.[i] in
    let {real_val; str_val} = simplified_test in
    if List.hd str_val = current_char then
      let new_acc = real_val :: acc in
      check_based_on_list (i - 1) new_acc str
    else
      check_based_on_list (i - 1)  acc str
  else
    acc


let test_res = check_based_on_list 0 [] "9";;


(* let test_char_list = List.map generate_char_list_for_str test_str_list;; *)
(* let () = read_char_lines test_char_list;; *)



(* main test *)

let rec test_if_included_base char list =
  match list with
  | [] -> None
  | head :: tail ->
      if head = char then
        Some char
      else test_if_included_base char tail

let test_if_included char = test_if_included_base char list_of_possible;;

let rec test_str_includes_base i acc str =
  if i < String.length str then
    let char_code = Char.code str.[i] in
    let is_possible = test_if_included char_code in
    match is_possible with
    | Some valid_value ->
        let new_acc = valid_value :: acc in
        test_str_includes_base (i + 1) new_acc str
    | None -> test_str_includes_base (i + 1) acc str
  else
    acc

let test_str_includes str = test_str_includes_base 0 [] str;;

let rec included_by_newline acc list =
  match list with
  | [] -> acc
  | hd :: tl ->
      let is_included = test_str_includes hd in
      let new_acc = is_included :: acc in
      included_by_newline new_acc tl


let possible_test = included_by_newline [] input_new_line_split

let () = read_char_lines possible_test;;


let _sum_single_char_list list =
  let list_end = List.length list - 1 in
  let first_item = List.nth list list_end in
  let last_item = List.nth list 0 in
  first_item * 10 + last_item;;



print_newline ();;


(* let _possible_test = included_in_possible (List.nth input_new_line_split 0) 1;; *)

let _test = "this is a test";;

