(**
   Representation of dynamic scrabble state.
   This module represents the state of a scrabble board as it is being played,
   including the blocks that have landed, the rows that have been cleared,
   the blocks that are filled on the board, and the functions that cause the
   state to change.
*)
open Dictionary
open TreeDictionary

type letter = char
type status = Filled | Unfilled
type coordinate = (int * int)
type block = Block of letter * status * coordinate
type board = (block array) array
type score = int
type tiled = letter * int * int

exception IllegalMove

(** [state] is the abstract type representing the game's state. *)
type state = {
  board: board;
  player_score: score;
  opponent_score: score;
  player_tiles: char list;
  total_tiles: (char*int) list;
  opponent_tiles: char list;
  midtiles: tiled list;
  prev_play: tiled list;
}

module IntKeyVal : ValueSig

module StringKeyVal : KeySig

module StringTreeDict : Dictionary with module Key = StringKeyVal 
                                    and module Value = IntKeyVal

type state_result = Valid of state | Invalid

(** [init_state x y] returns an initialized state object with board of 
    dimensions [x] by [y]. *)
val init_state : int -> int -> state_result

(** [draw_board s] draws the game interface based on game state [s]. *)
val draw_board : state -> unit

(** [tile_state c s x] returns the resulting state object after executing
    command [c] with command type [x] on state [s] *)
val tile_state : string list -> state -> int -> state_result

(** [play_word_state c s] returns the resulting state object after playing
    word based on command [c] on state [s] *)
val play_word_state : state -> state_result

(** [play_bot_turn s] calculates and prompts the AI bot's best move on state 
    [s]. *)
val play_bot_turn : state -> state_result

(** [discard_tile_state s] returns the new state after removing a dictated tile 
    from distributed tiles. *)
val discard_tile_state : string list -> state -> state_result

(** [get_game_winner s] returns the winner of the game signified by state [s]. *)
val get_game_winner : state -> string

(** [shuffle_tiles_state s] returns a new state after shuffling the player's 
    tiles. *)
val shuffle_tiles_state : state -> state_result

(** [words_in_board board] gets all possible words in board. *)
val words_in_board : board -> string list

(** [valid_word word] checks if the word [word] exists in the dictionary.  *)
val valid_word : StringTreeDict.key -> bool

