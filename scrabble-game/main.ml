open State
open Command
open Dictionary
open TreeDictionary

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

(** [prompt()] prompts for a command and asks the player to enter a new valid
    command. Tile placement commands must be of format (TILE X-COORD Y-COORD) *)
let prompt() = print_newline(); print_string ">"

(** [print_instructions()] prints the instructions to the user, including all 
  possible moves and directions on how to play them. *)
let print_instructions() =
  (* UNIMPLEMENTED *)
  ANSITerminal.(print_string [cyan] "\n ============================== GAME INSTRUCTIONS ==============================\n");
  ANSITerminal.(print_string [cyan] "Place tiles on the board to form words.\nAll tiles must be connected horizontally or vertically and form valid words in every direction.\n\n");
  print_string "- PLACE any tile in your possession on the board by entering: \n";
  ANSITerminal.(print_string [cyan] "   > 'place (letter) (x-coord) (y-coord)' \n");
  print_string "- REMOVE any tile you've placed on the board in the current turn by entering: \n";
  ANSITerminal.(print_string [cyan] "   > 'remove (letter) (x-coord) (y-coord)' \n");
  print_string "- RECALL all tiles placed in the current turn by entering: \n";
  ANSITerminal.(print_string [cyan] "   > 'recall' \n");
  print_string "- SHUFFLE your deck to reorder tiles by entering: \n";
  ANSITerminal.(print_string [cyan] "   > 'shuffle' \n");
  print_string "- DISCARD a tile in your possession to receive a new one from the tile bag by entering: 'discard (letter)'\n";
  ANSITerminal.(print_string [cyan] "   > 'discard (letter)' \n");
  print_string "- PLAY the word you've placed on the board by entering: \n";
  ANSITerminal.(print_string [cyan] "   > 'play' \n");
  print_string "- QUIT the game by entering \n";
  ANSITerminal.(print_string [cyan] "   > 'quit' \n");
  ANSITerminal.(print_string [yellow] "\nYou may see these instructions at any time using 'help'.\nType 'okay' to resume game.\n");
  ANSITerminal.(print_string [cyan] "================================================================================\n\n");
  print_string "> "
  (* print_newline() *)

(** [print_illegal_warning()] prints the illegal move warning and reprompts
  for another move. *)
let print_illegal_warning() = 
  print_string "Illegal command. Please try again.\nType 'help' to see the instructions again.\n"; 
  print_string "> "

(** [print_bot_warning()] prints the illegal move of a bot warning and 
  prompts user for another move. *)
let print_bot_warning() = 
  print_string "Bot skipped his turn - your move. \n"; 
  print_string "> "

let print_game_winner w = 
  if w = "player" 
    then print_string "CONGRATS ON WINNING. YOU ARE THE TRUE WORD MASTER! \n\n"
  else if w = "opponent" 
    then print_string "Ouch! Seems like you were beaten by our bot. \n Better luck next time! \n\n"
  else print_string "Wow, you're right on par with our bot! With more practice, you might even win next time! \n\n"

(**[play_scrabble state] reads the command typed by the player and responds 
    accordingly.
   - The game ends if the player types in "quit".
   - A new prompt is printed if the input command is invalid.
   - The user may place a tile at a specific coordinate, remove a tile, recall 
     all tiles, play the word, or quit the game.
*)
let rec play_scrabble state =
  let command = read_line() in 
  match parse command with
  | Quit   -> end_game state
  | Play -> play_tile state
  | Recall -> tile_change state [] 2 
  | Place tile  -> tile_change state tile 0
  | Remove tile -> tile_change state tile 1
  | Help -> print_instructions(); play_scrabble state
  | Discard tile -> discard_tile tile state;
  | Shuffle -> shuffle_tiles state;
  | Okay -> redraw_board state;
  | exception Empty -> print_illegal_warning(); play_scrabble state
  | exception Malformed -> print_illegal_warning(); play_scrabble state
  | _ -> play_scrabble state

(** [tile_change state tile_cmd move_type] responds to any single tile moves,   
  executing the command [cmd] based on move type [mov] to state [state]. *)
and tile_change state tile_cmd move_type =
  let y = tile_state tile_cmd state move_type in 
  match y with 
  | Valid st -> State.draw_board st; print_string "> "; play_scrabble st
  | Invalid -> print_illegal_warning(); play_scrabble state

(** [bot_turn state] executes the bot's move on state [state]. *)
and bot_turn state = 
  let st' = play_bot_turn state in
  match st' with 
  | Valid st -> State.draw_board st; print_string "> "; play_scrabble st
  | Invalid -> print_bot_warning(); play_scrabble state

(** [play_tile cmd state] responds to play of a word, executing command [cmd] 
  on state [state]. *)
and play_tile state =
  let played_state = play_word_state state in 
  match played_state with
  | Valid st -> State.draw_board st; print_string "> "; bot_turn st
  | Invalid -> print_illegal_warning(); play_scrabble state

(** [discard cmd st] discards the specifed tile in command [cmd] on state 
[state]. *)
and discard_tile tile_cmd state = 
  let discarded_state = discard_tile_state tile_cmd state in
  match discarded_state with 
  | Valid st -> State.draw_board st; print_string "> "; play_scrabble st
  | Invalid -> print_illegal_warning(); play_scrabble state

(** [shuffle_tiles st] shuffles the player tiles in state [state]. *)
and shuffle_tiles state = 
  let shuffled_state = shuffle_tiles_state state in 
  match shuffled_state with 
  | Valid st -> State.draw_board st; print_string "> "; play_scrabble st
  | Invalid -> print_illegal_warning(); play_scrabble state


(** [redraw_board state] redraws the current board in state [state]*)
and redraw_board state = 
  State.draw_board state; print_string "> "; play_scrabble state

(** [end_game st] responds to play of a word, executing command [cmd] 
  on state [st]. *)
and end_game state = 
  let winner = get_game_winner state in 
  print_game_winner winner;
  Pervasives.exit 0
  
(** [init_board c] initializes the game based on command [c]. If [c] is 
    an invalid command, reprompt for a new command. *)
let rec init_board config_command =
  match (parse config_command) with
  | Init config -> let init_dimension = get_cmd_dimen config in
    let i = List.hd init_dimension in
    let j = List.hd (List.rev init_dimension) in
    let init_state_result = State.init_state i j in 
    (match init_state_result with 
     | Valid st -> 
       (draw_board st;
        print_string "> ";
        play_scrabble st)
     | Invalid -> (print_illegal_warning(); init_board (read_line())))
  | exception Empty -> print_illegal_warning(); init_board (read_line())
  | exception Malformed -> print_illegal_warning(); init_board (read_line())
  | _ -> print_string  "> "; init_board (read_line())

(** [main ()] prompts for the game to play and then starts the game. *)
let main () =
  ANSITerminal.(print_string [blue] "\n\nWelcome to the 3110 Words With Friends Game.\n");
  ANSITerminal.(print_string [red] "Please initialize your scrabble board with desired dimensions.\n");
  ANSITerminal.(print_string [red] "Tile dimensions must be between 4 and 10.\nEnter 'help' for instructions at any point in the game.\n");
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | init_command -> init_board init_command

(* Execute the game engine. *)
let () = main ()