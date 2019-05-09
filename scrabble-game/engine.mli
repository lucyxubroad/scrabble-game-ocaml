open State
open TreeDictionary
open Dictionary

type idx

module IntKeyVal : KeySig

module StringKeyVal :  ValueSig

module type StringTreeDict = 
  functor(K: KeySig) -> functor(V: ValueSig)
    -> Dictionary with module Key = K and module Value = V


(* val valid_words : StringTreeDict.key list -> bool *)

(* val valid_board: board -> bool  *)

val words_in_board1 : string list -> string list -> string list

val get_lines: 'a array array -> 'a list list

val chars_to_strs: char list list -> string list list -> string list list

val index_of_dir: string -> idx
