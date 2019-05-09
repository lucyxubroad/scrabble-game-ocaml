open Dictionary
open TreeDictionary


(* exception IllegalMove *)

(** [board_result] represents the result of an attempted action on the 
    board. *)
(* type board_result = Legal of board | Illegal *)

module IntKeyVal = struct
  type t = int
  let compare x y =
    match Pervasives.compare x y with
    | x when x<0 -> LT
    | 0 -> EQ
    | _ -> GT
end

module StringKeyVal = struct
  type t = string
  let compare x y =
    match Pervasives.compare x y with
    | x when x<0 -> LT
    | 0 -> EQ
    | _ -> GT
end

module StringTreeDict = TreeDictionary.Make(StringKeyVal)(IntKeyVal)

let read_f d =
  match (Unix.readdir d) with
  | entry -> Some entry
  | exception End_of_file -> None

(** [extract_txt dir d acc] is the list [acc] of .txt file names in  
    directory [d], opened directory descriptor [dir]. *) 
let rec extract_txt dir d (acc: string list) =
  match read_f dir with
  | Some x -> if (Filename.check_suffix x ".txt")
    then extract_txt dir d ((d^Filename.dir_sep^x)::acc)
    else extract_txt dir d (acc)
  | None -> Unix.closedir dir; acc

(** [read_txt t] is the string option containing the string of all 
    characters read until the end of valid input channel line [t]
    is reached. *)
let read_txt t =
  match (Pervasives.input_line t) with
  | l -> Some l
  | exception End_of_file -> None

(* [extract_lines text acc] is the string list containing every line 
   of a text file as a string element in the list. *)
let rec extract_lines text (acc: string list) =
  match read_txt text with
  | Some x -> extract_lines text (x::acc)
  | None -> Pervasives.close_in text; acc

(** [words line] is the list of words that exist in the 
    string [line].*)
let words line = String.trim line

(* Helper for [words_from_file txt] *)
let rec words_in_a_file lines accu = 
  match lines with 
  | [] -> accu
  | h::t -> words_in_a_file t ((words h)::accu) 

(** [words_in_file txt_file] is a list of all valid words in a file 
       with name [txt_file].*)
let words_from_file txt = 
  let lines = extract_lines (Pervasives.open_in txt) [] in 
  words_in_a_file lines []

(** [word idx_lst accu] is a list of all keys [accu] of a list of 
    key-value pairs [idx_lst].  *)
let rec word idx_lst accu : string list = 
  match idx_lst with 
  | [] -> accu 
  | (k,v)::t -> k::word t accu

let words idx  = word (StringTreeDict.to_list idx)

let rec words_dict (words: string list) (accu) = 
  match words with 
  | [] -> accu
  | h::t ->  words_dict t (StringTreeDict.insert h (String.length h) accu)

(** [index_of_dir d] is an index of the files in [d].  Only files whose
    names end in [.txt] are indexed.  Only [d] itself, not any
    of its subdirectories, is indexed.
    Raises: Not_found if [d] is not a valid directory. *)
let index_of_dir d = 
  let txt_files = extract_txt (Unix.opendir d) d [] in
  let words_lst = words_from_file (List.hd (txt_files)) in
  words_dict words_lst StringTreeDict.empty

(* [chars_to_str char_list accu] turns a list of chars [char_list] to a list of 
   strings [accu] *)
let rec chars_to_str (char_list) (accu) = 
  match char_list with
  | [] -> accu
  | h::t -> chars_to_str t ((String.make 1 h)::accu)

(* Helper that applies [chars_to_str] to a list of char lists.  *)
let rec chars_to_strs (char_lst_lst) (accu) = 
  match char_lst_lst with 
  | [] -> accu 
  | h::t -> chars_to_strs t ((List.rev (chars_to_str h []))::accu)

(* [strs] concatenates a list of string lists into a single string.  *)
let rec strs (str_lst_lst: string list list) accu : string list=
  match str_lst_lst with 
  | [] -> accu
  | h::t -> strs t ((String.concat "" h)::accu)

(* helper that turns the 2d-array of board to 
   a 2d-list representation. board_list = Array.to_list board *)
let rec arr_to_list (board_list) accu = 
  match board_list with 
  | [] -> accu
  | h::t -> arr_to_list t ((Array.to_list h)::accu)

let rec arr_to_list2 board = 
  arr_to_list (Array.to_list board) []

let rec get_col board_list x accu  =
  match board_list with 
  | [] -> accu 
  | h::t -> get_col t x (List.nth h x::accu)  

let rec get_cols (board_list) i accu =
  (* print_string "get_cols List.hd \n";  *)
  let bound = (List.length (List.hd board_list)) in 
  if i = bound then accu 
  else get_cols board_list (i+1) ((get_col board_list i [])::accu)


(* turn a char list into a string list *)
let rec chars_to_str (char_list) (accu: string list) = 
  match char_list with
  | [] -> accu
  | h::t -> chars_to_str t ((String.make 1 h)::accu)

let rec chars_to_strs (char_lst_lst) (accu) = 
  match char_lst_lst with 
  | [] -> accu 
  | h::t -> chars_to_strs t ((List.rev (chars_to_str h []))::accu)

let rec strs (str_lst_lst: string list list) accu : string list=
  match str_lst_lst with 
  | [] -> accu
  | h::t -> strs t ((String.concat "" h)::accu)

(* [prewords line] gets a list of possible words from a string ,i.e. with 
   whitespace truncated.  *)
let prewords line = Str.split (Str.regexp "[ \n\r\x0c\t]+") line

(* Helper that applies prewords to a list of strings.  *)
let rec prewords_lst pres accu = 
  match pres with 
  | [] -> accu
  | h::t -> prewords_lst t ((prewords h)::accu) 

let total_pres pres =
  List.concat (prewords_lst pres [])

let rec str_to_char s =  List.init (String.length s) (String.get s)

let rec str_to_char1 char_lst accu = 
  match char_lst with 
  | [] -> accu 
  | h::t -> str_to_char1 t (accu@[Char.lowercase_ascii h])

let rec str_to_chars s = 
  str_to_char1 (str_to_char s) []

let rec extract_letters (block_list) accu = 
  match block_list with 
  | [] -> accu
  | Block(x,_, _)::t -> extract_letters t (x::accu)

let rec extract_letters1 lsts accu = 
  match lsts with 
  | [] -> accu 
  | h::t -> extract_letters1 t (List.rev (extract_letters h [])::accu)

let rec all_letters board = 
  let lsts = get_lines board in 
  extract_letters1 lsts [] 

(* all possible words in board *)
let rec words_in_board board = 
  let chars_lst = all_letters board in 
  let strings_lst = (chars_to_strs chars_lst []) in 
  let string_lst = strs strings_lst [] in
  total_pres string_lst 

let rec words_in_board1 string accu = 
  match string with 
  | [] -> accu
  | h::t -> if String.length h > 1 
    then words_in_board1 t ((String.lowercase_ascii h)::accu)
    else words_in_board1 t accu

let valid_word word = 
  StringTreeDict.member word (index_of_dir "words")

let rec valid_words words_in_board = 
  match words_in_board with 
  | [] -> true
  | h::t -> valid_word h && valid_words t

(* let valid_board (board) = 
   valid_words (words_in_board1 (words_in_board board) [])  *)
