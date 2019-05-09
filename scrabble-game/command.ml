(* Note: You may introduce new code anywhere in this file. *) 
open State 

type object_phrase = string list

type command = 
  | Place of object_phrase
  | Remove of object_phrase
  | Init of object_phrase
  | Play 
  | Discard of object_phrase
  | Shuffle
  | Recall
  | Quit
  | Help
  | Okay

exception Empty 

exception Malformed

exception InvalidTile

(**[str_lst str] is the list of lower-case words from the string [str] with 
   all whitespace removed.*)
let str_lst str = 
  let  s = str |> String.trim |> String.lowercase_ascii 
           |> String.split_on_char ' ' in
  List.filter (fun x -> x <> "") s

(** [is_int i] is the int of input string [i]. Raises Malformed exception if [i]
  is not a valid string character, i.e. alphabetical and not numerical. *)
let is_int (i: string) = 
  match int_of_string i with 
  | exception Failure(_) -> raise Malformed 
  | _ -> int_of_string i 

let get_cmd_dimen cmd = match cmd with 
  | [i; j] -> [is_int i ; is_int j]
  | _ -> raise Malformed

(* the [string_or_int] type represents the union of int and string types. *)
type string_or_int = 
  | String of string 
  | Int of int

(** [get_cmd_format cmd] is the string_or_int list representation of string 
    list [cmd]. Raises Malformed exception when string list is not exactly 
    three elements. *)
let get_cmd_format cmd = 
  match cmd with 
  | [i; j; k] -> let y = is_int j in 
    let z = is_int k in [String i; Int y; Int z]
  | _ -> raise Malformed 

(** [is_valid_cmd cmd] is the boolean value determining whether command [cmd]
    is a valid string_or_int list consisting of a string and two ints 
    in that order. *)
let is_valid_cmd cmd = 
  let cmd_format = get_cmd_format cmd in 
  match cmd_format with 
  | [String x; Int y ; Int z] -> true
  | _ -> false

(** [get_cmd_format cmd] is the string_or_int list representation of string 
    list [init]. Raises Malformed exception when string list is not exactly 
    two elements. *)
let get_init_format cmd = 
  match cmd with
  | [i; j] -> let x = is_int i in let y = is_int j in [Int x; Int y]
  | _ -> raise Malformed 

(** [is_valid_init cmd] is the boolean value determining whether command [cmd]
    is a valid string_or_int list consisting of two ints. *)
let is_valid_init cmd = 
  let init_format = get_init_format cmd in
  match init_format with
  | [Int x; Int y] -> true
  | _ -> false


let parse str : command =
  let s_lst = str_lst str in
  match s_lst with
  |[] -> raise Empty
  |"place"::t   -> if ((List.length t <> 3) || (is_valid_cmd t = false)) 
    then raise Malformed else Place t
  |"remove"::t  -> if t = [] then raise Malformed else Remove t
  |"init"::t    -> if ((t = []) || (is_valid_init t = false))
   then raise Malformed else Init t
  |"play"::t    -> if t <> [] then raise Malformed else Play 
  |"recall"::t  -> if t <> [] then raise Malformed else Recall
  |"quit"::t    -> if t <> [] then raise Malformed else Quit
  |"shuffle"::t -> if t <> [] then raise Malformed else Shuffle
  |"help"::t    -> if t <> [] then raise Malformed else Help
  |"discard"::t -> if t = [] then raise Malformed else Discard t
  |"okay"::t    -> if t <> [] then raise Malformed else Okay
  | _ :: t      -> raise Malformed

