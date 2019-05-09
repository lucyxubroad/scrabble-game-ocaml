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

(** [board_result] represents the result of an attempted action on the 
    board. *)
type board_result = Legal of board | Illegal

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


(** [read_f d] is the next entry option in the directory [d].*)
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

(** [words_in_file txt] is a list of all words in a dictionary file 
    with name [txt].*)
let words_from_file txt = 
  let lines = extract_lines (Pervasives.open_in txt) [] in 
  words_in_a_file lines []

(** [word idx_lst accu] is a list of all keys [accu] of a list of 
    key-value pairs [idx_lst].  *)
let rec word idx_lst accu : string list = 
  match idx_lst with 
  | [] -> accu 
  | (k,v)::t -> k::word t accu

(* [words idx] ia a list of all words in dictionary [idx]. *)
let words idx  = word (StringTreeDict.to_list idx)

(* [words_dict words accu] insert all words in the list [words] into an 
   empty dictionary [accu]. *)
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

(** [tile_pt_value] represents the point delegations of each tile. *)
let tile_pt_value = [('A',1); ('B',3); ('C',3); ('D',2); ('E',1); ('F',4);
                     ('G',2); ('H',4); ('I',1); ('J',8); ('K',5); ('L',1); 
                     ('M',2); ('N',1); ('O',1); ('P',3); ('Q',10); ('R',1);
                     ('S',1); ('T',1); ('U',1); ('V',4); ('W',4); ('X',8); 
                     ('Y',4);('Z',10)]

(* [num_tiles] represents the remaining available tiles in the game. *)
let num_tiles = [('D',4); ('E',12); ('F',2); ('A',9); ('B',2); ('C',2);
                 ('G',3); ('H',2); ('P',2); ('Q',1); ('R',6); ('I',9); 
                 ('J',1); ('K',4); ('L',4); ('M',2); ('N',6); ('O',8);
                 ('S',4); ('V',2); ('U',4); ('W',2); ('X',1); ('T',6);
                 ('Z',1); ('Y',2)]


(* [dictionary] is the dictionary loaded for the game. *)
let word_dictionary = index_of_dir "words"

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

(** [state_result] represents the result of an attempted movement on 
    the state. *)
type state_result = Valid of state | Invalid 

(** [init x y] initializes an empty board with [x] rows and [y] columns. *)
let init_board x y = 
  let empty_block = Block(' ', Unfilled, (-1,-1)) in
  let init_board = Array.make_matrix x y empty_block in
  init_board

(** [tiles_to_string t] returns the string representation of player tiles 
    [t]. *)
let tiles_to_string t = 
  String.concat ", " (List.map (String.make 1) t)

(** [tilebag_to_string t] returns the string representation of tilebag [t]. *)
let rec tilebag_to_string acc tb = 
  match tb with 
  | [] -> acc
  | (l, i) :: t -> let character = (String.make 1 l) in
    let number = string_of_int i in 
    let accumulated = ("(" ^ character ^ ", " ^ number ^ ")") :: acc in 
    tilebag_to_string accumulated t 

(** [tiles_to_string a t] returns the string representation of tiled pieces 
    [t]. *)
let rec tiled_to_string acc m = 
  match m with 
  | [] -> acc
  | (l, i, j) :: t -> let character = (String.make 1 l) in 
    let row = string_of_int i in 
    let col = string_of_int j in 
    let accumulated = ("(" ^ character ^ ", " ^ row ^ ", " ^ col ^ ")")::acc in 
    tiled_to_string accumulated t 

(** [state_to_string s] returns the string representation of state [s]. *)
let state_to_string (s : state) =
  let list_state = [] in
  let score_string = ("Player Score: " ^ string_of_int s.player_score) in 
  let sl_state = score_string :: list_state in 
  let opponent_score_string = ("Opponent Score: " ^ string_of_int 
                                 s.opponent_score) in 
  let os_state = opponent_score_string :: sl_state in 
  let player_tiles_string = ("Player Tiles: " ^ tiles_to_string 
                               s.player_tiles) in 
  let pl_state = player_tiles_string :: os_state in
  let opponent_tiles_string = ("Opponent Tiles: " ^ tiles_to_string 
                                 s.opponent_tiles) in 
  let ol_state = opponent_tiles_string :: pl_state in 
  let remaining_tiles_string = ("Remaining Tiles: " ^ 
                                (String.concat " " (tilebag_to_string [] s.total_tiles))) in 
  let rl_state = remaining_tiles_string :: ol_state in
  let midtiles_string = ("Midiles: " ^ 
                         (String.concat " " (tiled_to_string [] s.midtiles))) in 
  let ml_state = midtiles_string :: rl_state in
  let prev_string = ("Prev Midiles: " ^ 
                     (String.concat " " (tiled_to_string [] s.prev_play))) in 
  let pml_state = prev_string :: ml_state in
  pml_state

(** [draw_scoreboard p o] draws the scoreboard displaying [p] for player's score
    and [o] for the opponent's score. *)
let draw_scoreboard player opponent = 
  print_string "🔴🔴🔴🔴🔴🔴🔴🔴🔴🔴🔴🔴🔴    🔴🔴🔴🔴🔴🔴🔴🔴🔴🔴🔴🔴🔴 \n";
  print_string "            👦                        🤖             \n";
  let ps = string_of_int player in
  let os = string_of_int opponent in
  print_string ("            " ^ ps ^ "                         " ^ os ^ "           \n");
  print_string "🔴🔴🔴🔴🔴🔴🔴🔴🔴🔴🔴🔴🔴    🔴🔴🔴🔴🔴🔴🔴🔴🔴🔴🔴🔴🔴\n\n"  

(** [draw_tiles_list t] draws the tileboard displaying [t] for player's tiles. *)
let draw_tiles_list t = 
  let list_len = (List.length t)-1 in
  for i = 0 to list_len do
    print_string " ➕➖➖➕ "
  done;
  print_string "\n";
  for i = 0 to list_len do
    let c = List.nth t i in 
    let l = String.make 1 c in
    print_string (" |   " ^ l ^ "  | ");
  done;
  print_string "\n";
  for i = 0 to list_len do
    print_string " ➕➖➖➕ "
  done;
  print_string "\n"

(** [draw_board s] draws the entire game interface according to the game state 
    [s]. *)
let draw_board s = 
  draw_scoreboard s.player_score s.opponent_score;
  print_string "\n";
  let b = s.board in
  for i = 0 to Array.length b - 1 do
    for j = 0 to Array.length b.(i) - 1 do 
      print_string " ⚪⚪⚪⚪⚪ "
    done;
    print_newline();
    for k = 0 to Array.length b.(i) - 1 do 
      let row_str = string_of_int i in
      let col_str = string_of_int k in
      let coord = row_str ^ "," ^ col_str in
      print_string (" ⚪ " ^ coord ^ "  ⚪ ");
    done;
    print_newline();
    for l = 0 to Array.length b.(i) - 1 do 
      print_string " ⚪      ⚪ ";
    done;
    print_newline();
    for m = 0 to Array.length b.(i) - 1 do 
      let curr_block = b.(i).(m) in
      match curr_block with
      | Block(l, Filled, _) -> ANSITerminal.(print_string [green] (" ⚪   " ^ (String.make 1 l) ^ "  ⚪ "));
      | Block( _, Unfilled, _) -> print_string " ⚪      ⚪ ";
    done;
    print_newline();
  done;
  for n = 0 to Array.length b.(0) - 1 do 
    print_string " ⚪⚪⚪⚪⚪ ";
  done;
  print_string "\n\n";
  draw_tiles_list s.player_tiles

(** [update_num_tiles c a t] returns the updated tilebag after removing char
    [c] from tilebag [t]. *)
let rec update_num_tiles char acc tile_list =
  match tile_list with
  | [] -> acc
  | (c, n) :: t -> if c = char && n > 1
    then update_num_tiles char ((c, n-1)::acc) t
    else if c = char then update_num_tiles char acc t
    else update_num_tiles char ((c, n)::acc) t 

(** [deal_single_tile t] returns a single char tile from the tilebag [t]. *)
let deal_single_tile t =
  let len_num_tiles = List.length t in
  let rand_index = Random.int len_num_tiles in
  let l_tile = List.nth t rand_index in 
  match l_tile with
  | (l, _) -> l

(** [deal_tiles n (l, t) b] returns a tuple containing list of tiles and 
    new tilebag after removing [n] tiles from tilebag [b]. *)
let rec deal_tiles n (l_acc, t_acc) t = 
  Random.self_init();
  if n = 0 then (l_acc, t_acc) else
    let l = deal_single_tile t in
    let t' = update_num_tiles l [] t in
    deal_tiles (n-1) ((l::l_acc), t') t'

(** [init_state x y] returns an initialized state object with board of 
    dimensions [x] by [y]. Reject any dimensions less than four and greater
    than 10. *)
let init_state x y = 
  if x > 10 || y > 10 || x < 4 || y < 4 then Invalid else
    let dealt_player = deal_tiles 8 ([], num_tiles) num_tiles in
    match dealt_player with
    | (l1, t1) -> let player_tiles = l1 in 
      let dealt_opponent = deal_tiles 8 ([], t1) t1 in
      match dealt_opponent with
      | (l2, t2) -> let opponent_tiles = l2 in
        let remaining_tiles = t2 in
        Valid {
          board = init_board x y;
          player_score = 0;
          opponent_score = 0;
          player_tiles = player_tiles;
          total_tiles = remaining_tiles;
          opponent_tiles = opponent_tiles;
          midtiles = [];
          prev_play = [];
        }

(** [char_in_tilebag c p] returns boolean indicating if char [c] is a 
    letter in player [p]'s tiles. *)
let rec char_in_tilebag char player_tiles = 
  match player_tiles with
  | [] -> false
  | h :: t -> if h = char then true 
    else char_in_tilebag char t

(** [is_valid_insert l i j s] returns a boolean indicating if inserting
    letter [l] at [i, j] in board in state [s] is legal for current human 
    player. *)
let is_valid_insert char i j (s:state) = 
  let x_dimen = Array.length s.board in
  let y_dimen = Array.length s.board.(0) in
  let valid_char = char_in_tilebag char s.player_tiles in
  let valid_coord = i >= 0 && j >= 0 && i < x_dimen && j < y_dimen in
  valid_char && valid_coord

(** [is_valid_opponent_insert l i j s] returns a boolean indicating if 
    inserting letter [l] at [i, j] in board in state [s] is legal for
    bot opponent player. *)
let is_valid_opponent_insert char i j (s:state) = 
  let x_dimen = Array.length s.board in
  let y_dimen = Array.length s.board.(0) in
  let valid_char = char_in_tilebag char s.opponent_tiles in
  let valid_coord = i >= 0 && j >= 0 && i < x_dimen && j < y_dimen in
  valid_char && valid_coord

(** [insert_midtiles l i j m] returns a new midtiles list after placing char
    [l] on the board at coordinates [(i, j)]. *)
let insert_midtiles (command: string list) midtiles = 
  match command with
  | [x; y; z] -> let l = Char.chr (Char.code (String.get x 0) - 32) in 
    let i = int_of_string y in
    let j = int_of_string z in
    (l, i, j) :: midtiles
  | _ -> midtiles 

(** [remove_midtiles l i j m] returns a new midtiles list after removing char
    [l] from the board at coordinates [(i, j)]. *)
let remove_midtiles (command: string list) midtiles = 
  match command with
  | [x; y; z] -> let l = Char.chr (Char.code (String.get x 0) - 32) in 
    let i = int_of_string y in
    let j = int_of_string z in
    List.filter (fun (x,y,z)  -> (x,y,z) <> (l,i,j)) midtiles 
  | _ -> midtiles

(** [recall_tiles midtiles s] returns a new board result after clearing all tiles 
    in the list [midtiles], which contains all tiles placed in current move. *)
let rec recall_tiles midtiles s =
  match midtiles with 
  | (l, i, j)::t ->
    s.board.(i).(j) <- Block(' ', Unfilled, (-1,-1)); recall_tiles t s ;
  | _ -> Legal s.board

(* Update midtiles *)
let update_midtiles command (s: state) move_type = 
  if move_type = 0 then insert_midtiles command s.midtiles
  else if move_type = 1 then remove_midtiles command s.midtiles
  else []

(** [insert_player_tile command player_tiles] is the new list of player tiles 
    after adding the a-z character in string list [command]. *)
let insert_player_tile command player_tiles =
  match command with 
  | [x; y; z] -> 
    let l = Char.chr (Char.code (String.get x 0) - 32) in 
    l::player_tiles 
  | _ -> player_tiles

(** [count_player_tile] is the number of occurrences of a letter [c] in the 
    character list [tile_list], which represetng the player tilebag. *)
let count_player_tile (c: char) (acc: int) (tile_list: char list) = 
  List.fold_left (fun acc x -> if x = c then (acc+1) else acc) acc tile_list

(** [remake_player_tiles char acc tile_list] is the player tile bag after 
    character [char] is removed from tile list [tile_list]. *)
let rec remake_player_tiles char acc tile_list = 
  match tile_list with 
  | []   -> acc
  | h::t -> let num_char = count_player_tile h 0 tile_list in 
    if h = char && num_char = 1 then remake_player_tiles char acc t 
    else remake_player_tiles char (h::acc) t 

(** [remove_player_tile command player_tiles] is the character list representing
    player tiles after removing the character in string list [command]. *)
let remove_player_tile command player_tiles = 
  match command with
  | [x; y; z] -> 
    let l = Char.chr (Char.code (String.get x 0) - 32) in 
    remake_player_tiles l [] player_tiles
  | _ -> player_tiles

(** [return_player_tiles midtiles acc] is the list of player tiles determined by 
    the currently played tiles in list [midtiles]. *)
let rec return_player_tiles midtiles acc =
  match midtiles with 
  | (l,i,j)::t -> return_player_tiles t (l::acc) ;
  | _ -> acc

(** [return_opponent_tiles] is the list of opponent tiles determined by the 
    currently played tiles in the list [midtiles]. *)
let rec return_opponent_tiles midtiles acc =
  match midtiles with 
  | (l,i,j)::t -> return_opponent_tiles t (l::acc) ;
  | _ -> acc

(** [update_player_tiles command midtiles s move_type] is the new list of 
    player tiles given command [command] and currently placed tiles [midtiles]
    in state [s], given a certain move type [move_type]. *)
let update_player_tiles command midtiles (s: state) move_type = 
  let player_tiles = s.player_tiles in 
  if move_type = 0 then remove_player_tile command player_tiles
  else if move_type = 1 then insert_player_tile command player_tiles
  else return_player_tiles s.midtiles player_tiles 

(** [update_opponent_tiles command midtiles s move_type] is the new list of 
    opponent tiles given command [command] and currently placed tiles [midtiles]
    in state [s], given a certain move type [move_type]. *)
let update_opponent_tiles command midtiles (s: state) move_type = 
  let opponent_tiles = s.opponent_tiles in 
  if move_type = 0 then remove_player_tile command opponent_tiles
  else if move_type = 1 then insert_player_tile command opponent_tiles
  else return_opponent_tiles s.midtiles opponent_tiles

(** [fill_tile l i j s] returns a new board after inserting char [l] onto board
    in state [s] at coordinates [(i, j).] *)
let fill_tile l i j (s: state) = 
  let board = s.board in
  if (is_valid_insert l i j s) then
    (match (board.(i).(j)) with
     | Block (_, Unfilled, _) -> 
       board.(i).(j) <- Block(l, Filled, (i,j)); Legal board
     | Block (_, Filled, _ ) -> Illegal)
  else Illegal

(** [fill_opponent_tile l i j s] returns a new board after inserting char [l] 
    onto board in state [s] at coordinates [(i, j)] for player.*)
let fill_opponent_tile l i j (s: state) = 
  let board = s.board in
  if (is_valid_opponent_insert l i j s) then
    (match (board.(i).(j)) with
     | Block (_, Unfilled, _) -> 
       board.(i).(j) <- Block(l, Filled, (i,j)); Legal board
     | Block (_, Filled, _ ) -> Illegal)
  else Illegal

(** [remove_tile l i j s] returns a new board after removing char [l] from board
    in state [s] at coordinates [(i, j).] *)
let remove_tile l i j (s: state) = 
  let board = s.board in
  (match (board.(i).(j)) with
   | Block (_, Unfilled, _) -> Illegal
   | Block (_, Filled, _) -> board.(i).(j) <- Block(' ', Unfilled, (-1,-1)); Legal board)

(** [tile_board command state move_type] returns the new board depending on
    the command [command] and type of move [move_type]. 
    - When move_type = 0, tiles are drawn on the board.
    - When move_type = 1, tiles are removed from the board.
    - If [command] is an empty list, all tiles are recalled.*)
let tile_board (command: string list) state move_type = 
  match command with
  | [x; y; z] -> 
    let l = Char.chr (Char.code (String.get x 0) - 32) in 
    let i = int_of_string y in
    let j = int_of_string z in
    if move_type = 0 then fill_tile l i j state
    else remove_tile l i j state
  | [] -> recall_tiles state.midtiles state
  | _ -> Illegal

(** [tile_opponent_board command state move_type] is the board returned after 
    the opponent places or removes a tile given by command [command] and 
    move type [move_type] in state [state]. *) 
let tile_opponent_board (command: string list) state move_type = 
  match command with
  | [x; y; z] -> 
    let l = Char.chr (Char.code (String.get x 0) - 32) in 
    let i = int_of_string y in
    let j = int_of_string z in
    if move_type = 0 then fill_opponent_tile l i j state
    else remove_tile l i j state
  | [] -> recall_tiles state.midtiles state
  | _ -> Illegal

(** [update_num_tiles char acc tile_list] is the updated tile bag given tile 
    [char] has been dealt from tile list [tile_list]. *)
let rec update_num_tiles char acc tile_list =
  match tile_list with
  | [] -> acc
  | (c, n) :: t -> if c = char && n > 1
    then update_num_tiles char ((c, n-1)::acc) t
    else if c = char then update_num_tiles char acc t
    else update_num_tiles char ((c, n)::acc) t 

(** [get_tile_value] is the value of tile [tile] obtained from points in 
    [tile_pt_list]. *)
let rec get_tile_value tile tile_pt_list = 
  match tile_pt_list with 
  | [] -> 0
  | (l, value)::t -> if (tile = l) then value else get_tile_value tile t

(** [score_calc midtiles acc] calculates the score of the played word given
    the currently placed tiles in list [midtiles]. *)
let rec score_calc midtiles acc = 
  match midtiles with 
  | [] -> acc
  | (x,y,z)::t -> score_calc t (acc + get_tile_value x tile_pt_value)

(** [tile_state cmd st mov] places a player's word onto the board in state [st]
    based on command [cmd] and move type [mov]. *)
let tile_state (command: string list) (s: state) move_type =
  let board' = tile_board command s move_type in
  let midtiles' = update_midtiles command s move_type in
  let player_tiles' = update_player_tiles command midtiles' s move_type in
  match board' with
  | Legal b' -> Valid {
      board = b';
      player_score = s.player_score;
      opponent_score = s.opponent_score;
      player_tiles = player_tiles';
      total_tiles = s.total_tiles;
      opponent_tiles = s.opponent_tiles;
      midtiles = midtiles'; 
      prev_play = s.prev_play;
    }
  | Illegal  -> Invalid

(** [tile_opponent_state cmd st mov] places a bot's word onto the board in 
    state [st] based on command [cmd] and move type [mov]. *)
let tile_opponent_state (command: string list) (s: state) move_type =
  let board' = tile_opponent_board command s move_type in
  let midtiles' = update_midtiles command s move_type in
  let opponent_tiles' = update_opponent_tiles command midtiles' s move_type in
  match board' with
  | Legal b' -> Valid {
      board = b';
      player_score = s.player_score;
      opponent_score = s.opponent_score;
      player_tiles = s.player_tiles;
      total_tiles = s.total_tiles;
      opponent_tiles = opponent_tiles';
      midtiles = midtiles'; 
      prev_play = s.prev_play;
    }
  | Illegal  -> Invalid


(* [filled st] is a set-like list of the tile identifiers the player has 
   filled in state [board]. The player has filled a tile [tl] if the tile has 
   even been put a letter on. *)
let rec filled_tiles st (accu: (int*int) list) = 
  let b = st.board in 
  for i = 0 to Array.length b - 1 do 
    for j = 0 to Array.length b.(i) -1 do 
      match (b.(i).(j)) with 
      | Block(l, Filled, _) -> filled_tiles st ((i,j)::accu)
      | Block(l, Unfilled, _) -> filled_tiles st accu
    done
  done  

(* Helper that turns the 2d-array of board to 
   a 2d-list representation. board_list = Array.to_list board *)
let rec arr_to_list (board_list: block array list) accu = 
  match board_list with 
  | [] -> accu
  | h::t -> arr_to_list t ((Array.to_list h)::accu)

(** [arr_to_list2 board] is the 2d-list version of a 2d-array [board]. *)
let rec arr_to_list2 board = 
  arr_to_list (Array.to_list board) []

(* [get_col board_list x accu] gets the [x]th column of a 2d-list [boardr_list]
   in a list form. *)
let rec get_col board_list x accu : block list =
  match board_list with 
  | [] -> accu 
  | h::t -> get_col t x (List.nth h x::accu)  

(* [get_cols board_list i accu] get all the columns from a 2d-list. i and accu 
   are accumulators, with i always being 0 and accu always being []. *)
let rec get_cols (board_list: block list list) i accu : block list list= 
  let bound = (List.length (List.hd board_list)) in 
  if i = bound then accu 
  else get_cols board_list (i+1) ((get_col board_list i [])::accu)

(* [get_lines board] gets a list of all rows and columns from 2d-array [board], 
   i.e. a list of lists.  *)
let get_lines (board: board) = 
  let board_lst = arr_to_list2 board in 
  let cols = get_cols (board_lst) 0 [] in 
  List.append cols board_lst

(* [extract_letters block_list accu] extracts a list of letters from from a 
   block list [block_list]. *)
let rec extract_letters (block_list: block list) accu : letter list = 
  match block_list with 
  | [] -> accu
  | Block(x,_, _)::t -> extract_letters t (x::accu)

(* Helper that applies [extract_letters] to a list of block lists.  *)
let rec extract_letters1 lsts accu = 
  match lsts with 
  | [] -> accu 
  | h::t -> extract_letters1 t (List.rev (extract_letters h [])::accu)

(* [all_letters board] extracts all the letters existing on the board [board]. *)
let rec all_letters board = 
  let lsts = get_lines board in 
  extract_letters1 lsts []

(* [chars_to_str char_list accu] turns a list of chars [char_list] to a list of 
   strings [accu] *)
let rec chars_to_str (char_list: letter list) (accu: string list) = 
  match char_list with
  | [] -> accu
  | h::t -> chars_to_str t ((String.make 1 h)::accu)

(* Helper that applies [chars_to_str] to a list of char lists.  *)
let rec chars_to_strs (char_lst_lst: letter list list) (accu: string list list) = 
  match char_lst_lst with 
  | [] -> accu 
  | h::t -> chars_to_strs t ((List.rev (chars_to_str h []))::accu)

(* [strs] concatenates a list of string lists into a single string.  *)
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

(* Helper that concatenate all lists got from [prewords_lst] together, returns 
   a final list of prewords. *)
let total_pres pres =
  List.concat (prewords_lst pres [])

(* [words_in_board board] gets all possible words in board. *)
let rec words_in_board board = 
  let chars_lst = all_letters board in 
  let strings_lst = (chars_to_strs chars_lst []) in 
  let string_lst = strs strings_lst [] in
  total_pres string_lst

(* [words_in_board1 board] excludes the words shorter than 2, and turns all words
   into lowercase form to match the dictionary.  *)
let rec words_in_board1 strings accu = 
  match strings with 
  | [] -> accu
  | h::t -> if String.length h > 1 
    then words_in_board1 t ((String.lowercase_ascii h)::accu)
    else words_in_board1 t accu

(* [valid_word word] checks if the word [word] exists in the dictionary.  *)
let valid_word word = 
  StringTreeDict.member word (word_dictionary)

(* [valid_words words_in_board] checks if a list of words are all in the 
   dictionary.  *)
let rec valid_words words_in_board  = 
  match words_in_board with 
  | [] -> true
  | h::t -> if not (valid_word h) then false else valid_words t  

(* [valid_board board] checks if a board is valid in terms of the words on it. 
   Used to validate player's move.  *)
let valid_board board = valid_words (words_in_board1 (words_in_board board) []) 

(* [valid_state st] checks whether a state is valid in terms of the validation 
   of the board.  *)
let valid_state st = (valid_board st.board)

(* retrieve all filled tiles from list of blocks on board *)
let rec get_filled_tiles acc (blocks: block list) = 
  match blocks with
  | [] -> acc
  | Block(l, stat, coord)::t -> if stat = Filled 
    then get_filled_tiles ((l,stat,coord)::acc) t
    else get_filled_tiles acc t 

(* get all filled tiles on board *)
let rec get_tiles_on_board board = 
  let board_list2d = arr_to_list2 board in 
  let board_list = List.concat board_list2d in
  get_filled_tiles [] board_list

(* check coordinates to see if they are adjacent *)
let check_adjacent x1 y1 x2 y2 = 
  if (x1 = x2) || (y1 = y2) then true else false 

(* check that two tiles are adjacent *)
let adjacent_tiles tile1 tile2 = 
  match tile1 with 
  | (l1, s1, c1) -> let (x1, y1) = c1 in 
    match tile2 with 
    | (l2, s2, c2) -> let (x2, y2) = c2 in 
      check_adjacent x1 y1 x2 y2

(* look for adjacent tiles, iterating through board *)
let rec look_for_adj_tiles midtile block_list = 
  match block_list with 
  | h::t -> if adjacent_tiles h midtile then h else look_for_adj_tiles midtile t 
  | _    -> midtile

(* use midtile to find adjacent tile *)
let adjacent_to_midtile midtiles board = 
  match midtiles with 
  | (l, s, c) -> look_for_adj_tiles (l,s,c) (get_tiles_on_board board)

(* convert midtile to lsc form *)
let tile_to_lsc midtile = 
  match midtile with 
  | (l, x, y) -> (l, Filled, (x,y))

(* convert from lsc to midtile *)
let lsc_to_tiled block = 
  match block with 
  | (l, s, (x,y)) -> (l, x, y)

(* update prev_play tiles *)
let update_prev midtiles (s:state) = 
  if List.length midtiles = 1 then 
    let one_tile = List.hd midtiles in 
    let block_midtile = tile_to_lsc one_tile in
    let adjacent_block = adjacent_to_midtile (block_midtile) s.board in 
    let adjacent_tile = lsc_to_tiled adjacent_block in 
    one_tile :: [adjacent_tile]
  else midtiles

(* [play_word_state] returns the state after playing the word [word] 
   on state [s] by the player. *)
let play_word_state (s: state) = 
  draw_board s;
  let board_validity = valid_state s in
  if board_validity && List.length s.midtiles > 0
  then 
    let board' = s.board in 
    let player_score' = score_calc s.midtiles s.player_score in 
    let replace_ptile_num = 8 - List.length s.player_tiles in
    let replace_otile_num = 8 - List.length s.opponent_tiles in
    match (deal_tiles replace_ptile_num ([], s.total_tiles) s.total_tiles) with
    | (l1, t1) -> let player_tiles' = (List.rev_append s.player_tiles l1) in 
      match (deal_tiles replace_otile_num ([],t1) t1) with
      | (l2, t2) -> let opponent_tiles' = (List.rev_append s.opponent_tiles l2) in
        let total_tiles' = t2 in
        let prev_midtiles = update_prev s.midtiles s in
        let midtiles' = [] in
        Valid {
          board = board';
          player_score = player_score';
          opponent_score = s.opponent_score;
          player_tiles = player_tiles';
          opponent_tiles = opponent_tiles';
          midtiles = midtiles';
          total_tiles = total_tiles';
          prev_play = prev_midtiles;
        }
  else Invalid

(* [play_opponent_word_state] returns the state after playing the word [word] 
   on state [s] by the opponent/bot. *)
let play_opponent_word_state word (s: state) = 
  let board_validity = valid_state s in
  if board_validity && List.length s.midtiles > 0
  then 
    let board' = s.board in 
    let opponent_score' = score_calc s.midtiles s.opponent_score in
    let replace_ptile_num = 8 - List.length s.player_tiles in
    let replace_otile_num = 8 - List.length s.opponent_tiles in
    match (deal_tiles replace_ptile_num ([], s.total_tiles) s.total_tiles) with
    | (l1, t1) -> let player_tiles' = (List.rev_append s.player_tiles l1) in 
      match (deal_tiles replace_otile_num ([],t1) t1) with
      | (l2, t2) -> let opponent_tiles' = (List.rev_append s.opponent_tiles l2) in
        let total_tiles' = t2 in
        let prev_midtiles = update_prev s.midtiles s in
        let midtiles' = [] in
        Valid {
          board = board';
          player_score = s.player_score;
          opponent_score = opponent_score';
          player_tiles = player_tiles';
          opponent_tiles = opponent_tiles';
          midtiles = midtiles';
          total_tiles = total_tiles';
          prev_play = prev_midtiles
        }
  else Invalid

(* [str_to_char s] turns a string [s] into a char list.  *)
let rec str_to_char s =  List.init (String.length s) (String.get s)

(* [str_to_char1 c a] turns all chars in the char list [c] into lowercase chars. *)
let rec str_to_char1 char_lst accu = 
  match char_lst with 
  | [] -> accu 
  | h::t -> str_to_char1 t (accu@[Char.lowercase_ascii h])

(* [str_to_chars s] turns a string into a lowercase char list.  *)
let rec str_to_chars s = 
  str_to_char1 (str_to_char s) []

(* [remove1 chars c accu] removes the first occurence of character [c] in the 
   char list [chars] and return a new char list [accu]. *)
let rec removefst chars (c:char) accu =  
  match chars with 
  | [] -> accu
  | h::t -> if h = (Char.lowercase_ascii c) then accu @ t 
    else removefst t (Char.lowercase_ascii c) (accu @ [h])

(* [get_filled_lst acc b] returns list of filled blocks on the board *)
let rec get_filled_lst (acc: block list) (b_list: block list) =
  match b_list with 
  | [] -> acc
  | Block(l, s, c)::t -> if s = Filled 
    then get_filled_lst (Block(l, s, c)::acc) t 
    else get_filled_lst acc t

(* [is_unfilled block] checks whether the block [b] is filled.  *)
let is_unfilled block = 
  match block with 
  | Block(l, Filled, _) -> false
  | Block (l, Unfilled, _) -> true

(* [get_num_free_before] gets the number of free blocks upon the tile
   (l,i,j).  *)
let rec get_num_free_before (l,i,j) a c p n = 
  let count = ref 0 in 
  for k = 0 to i-1 do
    if is_unfilled (List.nth c k) && is_unfilled (List.nth p k) 
       && is_unfilled (List.nth n k) then (count := !count +1) 
    else (count:=0)
  done;
  !count

(* [col_num_free_after] gets the number of free blocks below the tile
   (l,i,j).  *)
let rec col_num_free_after (l,i,j) a c p n = 
  let count = ref 0 in 
  for k = i+1 to a-1 do
    if is_unfilled (List.nth c k) && is_unfilled (List.nth p k) 
       && is_unfilled (List.nth n k) then (count := !count +1) else ()
  done;
  !count

(* [row_num_free_before] gets the number of free blocks to the left of the tile
   (l,i,j).  *)
let rec row_num_free_before (l,i,j) a c p n = 
  let count = ref 0 in 
  for k = 0 to j-1 do
    if is_unfilled (List.nth c k) && is_unfilled (List.nth p k) 
       && is_unfilled (List.nth n k) then (count := !count +1) 
    else (count:=0)
  done;
  !count

(* [row_num_free_after] gets the number of free blocks to the right of the tile
   (l,i,j).  *)
let rec row_num_free_after (l,i,j) a c p n = 
  let count = ref 0 in 
  for k = j+1 to a-1 do
    if is_unfilled (List.nth c k) && is_unfilled (List.nth p k) 
       && is_unfilled (List.nth n k) then (count := !count +1) else ()
  done;
  !count

(* [print_block list] prints the string representation of the block list [list]. *)
let rec print_block list = 
  match list with 
  | [] -> print_string "\n"
  | Block(_, _, (i,j)) :: t -> 
    print_string ("(" ^ (string_of_int i) ^ ", " ^ 
                  (string_of_int j) ^ ") "); print_block t 

(* [get_prev_col j b] gets the list representation of previous column of 
   [j]th column on the board [b]. *)
let get_prev_col j board =
  if j=0 then (get_col (arr_to_list2 board) (j) []) 
  else (get_col (arr_to_list2 board) (j-1) [])

(* [get_next_col j b] gets the list representation of next column of [i]th 
   column on the board [b]. *)
let get_next_col j board = 
  if j = (Array.length board.(0)-1) then (get_col (arr_to_list2 board) (j) []) 
  else (get_col (arr_to_list2 board) (j+1) [])

(* [get_prev_row i b] gets the list representation of previous row of [i]th row on 
   the board [b]. *)
let get_prev_row i board = 
  if i=0 then List.nth (arr_to_list2 board) (i) 
  else (List.nth (arr_to_list2 board) (i+1))

(* [get_next_row i b] gets the list representation of next row of [i]th row on 
   the board [b]. *)
let get_next_row i board = 
  if i=(Array.length board)-1 then List.nth (arr_to_list2 board) (i) 
  else (List.nth (arr_to_list2 board) (i-1))

(* [get_consec_free t o f b] gets the number of consecutive free blocks around 
   tile [t] based on orientation [o], list of all free blocks [f] on the board [b]. *)
let get_consec_free (l,i,j) orientation free_blocks board = 
  if orientation = "horizontal" then 
    let curr_col = get_col (arr_to_list2 board) j [] in
    let prev_col = get_prev_col j board in
    let next_col = get_next_col j board in
    let num_free_before = get_num_free_before (l,i,j) 0 
        curr_col prev_col next_col in 
    let num_free_after = col_num_free_after (l,i,j) (Array.length board) 
        curr_col prev_col next_col in
    if num_free_before > num_free_after 
    then ("before", num_free_before) 
    else ("after", num_free_after)
  else 
    let curr_row = List.nth (arr_to_list2 board) i in
    let prev_row = get_prev_row i board in
    let next_row = get_next_row i board in
    let num_free_before = row_num_free_before (l,i,j) 0 curr_row prev_row 
        next_row in 
    let num_free_after = row_num_free_after (l,i,j) (Array.length board.(0)) 
        curr_row prev_row next_row in
    if num_free_before > num_free_after then ("before", num_free_before) 
    else ("after", num_free_after)

(* [get_free] gets the list of all possible moves along with the number of 
   empty tiles around the target tile, based on previous word play [pred_word_play] *)
let rec get_free acc prev_word_play orientation free_blocks board = 
  match prev_word_play with 
  | [] -> acc
  | (l,i,j)::t -> let consec_free = get_consec_free (l,i,j) orientation free_blocks board in 
    get_free (((l,i,j),consec_free)::acc) t orientation free_blocks board

(* [get_orientation pred_word_play] gets the orientation of the previous move.  *)
let get_orientation (prev_word_play:tiled list) = 
  print_newline();
  let (l1,i1,j1) = List.hd prev_word_play in
  let rev_word_play = List.rev prev_word_play in
  let (l2,i2,j2) = List.hd rev_word_play in
  if i1=i2 then "horizontal" else "vertical"

(* [compare_free] compares two possible moves.  *)
let compare_free ((l1,s1,c1), (str1, i1)) ((l2,s2,c2), (str2, i2)) = 
  if i1 > i2 then 1 
  else if i2 > i1 then - 1 
  else 0 

(* [get_max_free free_list] gets the move with maximum blank tiles from 
   the list of all possible moves [free_list]. *)
let get_max_free free_list = 
  let temp_list = List.sort compare_free free_list in
  List.hd temp_list

(* [get_possible_moves_lst pred_word_play board orientation] returns the best
   move based on previous play [pred_word_play] on the board [board] based on 
   [orientation]. *)
let get_possible_moves_lst (prev_word_play: tiled list) board orientation =
  let two_d_board_list = arr_to_list2 board in
  let board_list = List.concat two_d_board_list in
  let filled_list = get_filled_lst [] board_list in
  let free = get_free [] prev_word_play orientation filled_list board in
  let max_free = get_max_free free in max_free


(* [moves] represents the predicted moves for AI bot.  *)
type moves = {
  letter : char;
  row : int;
  col : int;
  blanktype : string;
  numblank : int
}

(* Helper that turns the result of predicted move into a record.  *)
let get_types double_tuple = 
  match double_tuple with 
  |((l,i,j),(t,n)) ->
    {letter = l;row = i; col = j; blanktype = t; numblank = n}

(* get first or last character of the word depends on the movetype *)
let get_index move word = 
  if move.blanktype = "before" then String.get word (String.length word - 1) 
  else String.get word 0 

(* [lower chars accu] turns a list of chars into lowercase chars.  *)
let rec lower chars accu = 
  match chars with 
  | []-> accu 
  | h::t -> lower t accu@[Char.lowercase_ascii h] 

(* [contain_least word chars] checks if the word [word] contains at least one 
   character in the char list [chars]. *)
let rec contain_least word chars = 
  match chars with 
  | [] -> false
  | h::t-> if String.contains word h then true else contain_least word t

(* [contain_all word chars accu] checks if all the characters in word are also
   in the list of chars [chars]. *)
let rec contain_all word chars accu  = 
  match word with 
  | [] -> accu  
  | h::t -> if (List.mem h (lower (chars) [])) 
    then contain_all t chars (true && accu) else false

(* [word_contain w char chars] checks whether the word [word] contains at least one 
   character [char] in the char list [chars], and either its first or last letter 
   is equal to character [char], i.e. the selected root position on board.  *)
let rec word_contain (word: string) char (chars: char list)  = 
  contain_least word (lower (chars) [])
  && ((String.get word 0 = (Char.lowercase_ascii char) )
      || String.get word (String.length word - 1) = Char.lowercase_ascii char) 

(* [word_with_char_s word char chars length move] checks whether a word satisfies
   an AI move. It checks if a word [word] has either its first/last character same
   as [char], and its rest of characters are all in char list [chars], and is 
   equal to or shorter than the specified maximum length [length]. [move] is the 
   predicted move type.  *)
let word_with_chars (word: string) char chars (length:int) move = 
  (String.length word <= (length + 1)) &&
  (word_contain word char chars) 
  && ((get_index move word) = Char.lowercase_ascii char)
  && (contain_all (removefst (str_to_chars word) char []) chars true)

(* [words_choose char chars length dict_lst index] choose the first word 
   that satisfies the requirements specified by [word_with_chars]. *)
let rec words_choose char chars length dict_lst index   = 
  match dict_lst with 
  | [] -> ""
  | (k,v)::t -> if word_with_chars k char chars length index then k else 
      words_choose char chars length t index 

(* [final_words state orientation config] gets the valid word for AI bot.  *)
let final_word state orientation config=
  let move = get_types (config) in 
  words_choose move.letter state.opponent_tiles move.numblank 
    (StringTreeDict.to_list word_dictionary) move 


(** [print_bot_move m] prints the optimal AI bot move [m]. *)
let print_bot_move ((l,i,j),(s, f)) = 
  let character = String.make 1 l in 
  let row = string_of_int i in 
  let col = string_of_int j in 
  let direction = s in 
  let num_free = string_of_int f in 
  print_string "The AI's predicted best move is: ";
  print_string ("(" ^ character ^ ", " ^ row ^ ", " ^ 
                col ^ "), (" ^ direction ^ ", " ^ num_free ^ ") \n\n")

(** [simple_bot_move s] returns the optimal AI bot move given the player's 
    previous play based on state [s]. *)
let simple_bot_move (s:state) orientation = 
  let move = get_possible_moves_lst s.prev_play s.board orientation in move

(* [split_to_chr_list s] slits a string [s] into a list of characters. *)
let split_to_chr_list str =
  let rec split_list index c =
    if index < 0 then c else split_list (index - 1) (str.[index] :: c) in
  split_list (String.length str - 1) []

(** [tile_char_list c coord o s] tiles each character in char list [c] onto 
    the board in state [s] starting at coordinate [coord] in the orientation
    [o]. *)
let rec tile_char_list chr_list (i, j) orientation (s:state) = 
  match chr_list with 
  | [] -> s
  | h :: t -> let string_char = String.make 1 h in 
    let (ni, nj) = update_coord (i, j) orientation in 
    let ni_s = string_of_int ni in
    let nj_s = string_of_int nj in
    let s' = (tile_opponent_state [string_char; ni_s; nj_s] s 0) in
    match s' with 
    | Valid st -> (tile_char_list t (ni, nj) orientation st)
    | Invalid -> s

(** [update_coord coord orient] updates the coordinates [coord] based
    on tiling orientation [orient]. *)
and update_coord (i, j) orientation = 
  match orientation with 
  | ("horizontal", "after") -> (i+1, j)
  | ("horizontal", "before") -> (i-1, j)
  | ("vertical", "after") -> (i, j+1)
  | ("vertical", "before") -> (i, j-1)
  | _ -> (i, j)

(** [orient_word_char_list c o] orients the list of characters [c] based on 
    orientation [o] of word. *)
let orient_word_char_list char_list orientation = 
  match orientation with 
  | (_, "after") -> (match char_list with
      | h :: t -> t
      | _ -> char_list)
  | (_, "before") -> (match List.rev char_list with
      | h :: t -> t
      | _ -> List.rev char_list)
  | _ -> char_list

(** [simple_bot_play word coord orien st] executes the AI bot's best word 
    [word] on state [st] based on coordinates [coord] and orientation [orien] *)
let simple_bot_play word coord orientation (s: state) = 
  let word_char_list = split_to_chr_list word in
  let oriented_char_list = orient_word_char_list word_char_list orientation in
  let tiled_state = tile_char_list oriented_char_list coord orientation s in 
  let played_state = play_opponent_word_state [word] tiled_state in
  played_state

(** [play_bot_turn s] calculates and prompts the AI bot's best move on state 
    [s]. *)
let play_bot_turn (s:state) =
  let prev_word_play = s.prev_play in
  let orientation = get_orientation prev_word_play in
  let ((l, i, j), (str, num)) = simple_bot_move s orientation in 
  let best_word = final_word s orientation ((l, i, j), (str, num)) in
  let total_orientation = (orientation, str) in
  simple_bot_play best_word (i, j) total_orientation s

(* [get_game_winner s] returns the winner of the game signified by state [s]. *)
let get_game_winner (s:state) = 
  let player_score = s.player_score in
  let opponent_score = s.opponent_score in
  if (player_score > opponent_score) then "player"
  else if (player_score = opponent_score) then "draw"
  else "opponent"

(* [remove_element ele acc lst] removes the element [ele] from the list [lst]. *)
let rec remove_element ele acc lst = 
  match lst with 
  | [] -> acc
  | h::t -> if h=ele then (acc @ t) else remove_element ele (h::acc) t

(* [discard_tile_state s] returns the new state after removing a dictated tile 
   from distributed tiles. *)
let discard_tile_state (cmd: string list) (s: state) = 
  if ((List.length cmd) != 1) then Invalid 
  else (let char_string = List.nth cmd 0 in
        let char_discard = Char.chr (Char.code (String.get char_string 0) - 32) in
        let char_in_tiles = List.mem char_discard s.player_tiles in 
        if char_in_tiles then 
          (let player_tiles' = remove_element char_discard [] s.player_tiles in
           let (l', t') = deal_tiles 1 ([], s.total_tiles) s.total_tiles in 
           let player_tiles'' = (player_tiles' @ l') in
           Valid {
             board = s.board;
             player_score = s.player_score - 1;
             opponent_score = s.opponent_score;
             player_tiles = player_tiles'';
             opponent_tiles = s.opponent_tiles;
             midtiles = s.midtiles;
             total_tiles = t';
             prev_play = s.prev_play;
           })
        else Invalid)

(* [shuffle acc n l] shuffles the elements in list [l] [n] times.  *)
let rec shuffle acc n l = 
  if n = 0 then (acc @l) else 
    (let random = Random.int (List.length l) in 
     let char_selected = List.nth l random in 
     let l' = remove_element char_selected [] l in
     shuffle (char_selected::acc) (n-1) l')

(* [shuffle_tiles_state s] returns a new state after shuffling the player's 
   tiles. *)
let shuffle_tiles_state s = 
  let shuffled_tiles = shuffle [] 3 s.player_tiles in 
  Valid {
    board = s.board;
    player_score = s.player_score;
    opponent_score = s.opponent_score;
    player_tiles = shuffled_tiles;
    opponent_tiles = s.opponent_tiles;
    midtiles = s.midtiles;
    total_tiles = s.total_tiles;
    prev_play = s.prev_play;
  }
