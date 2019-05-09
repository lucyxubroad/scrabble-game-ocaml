open OUnit2
open State
open Command
open State


let command_tests = [
  "place1" >:: (fun _ -> assert_equal (Place ["a"; "2"; "3"]) 
                   (parse "place a 2 3"));
  "place2" >:: (fun _ -> assert_equal (Place ["i"; "3"; "10"]) 
                   (parse "place i          3               10"));
  "place3" >:: (fun _ -> assert_equal (Place ["r"; "100"; "132"]) 
                   (parse "place r 100            132"));
  "place4" >:: (fun _ -> assert_equal (Place ["x"; "11"; "12"]) 
                   (parse "pLaCe       x 11            12"));
  "place5" >:: (fun _ -> assert_equal (Place ["u"; "39"; "12"]) 
                   (parse "place u 39 12"));
  "place6" >:: (fun _ -> assert_raises Malformed 
                   (fun () -> parse "place   u             1212"));
  "place7" >:: (fun _ -> assert_raises Malformed 
                   (fun () -> parse "place   u   d  1212"));
  "place8" >:: (fun _ -> assert_raises Malformed 
                   (fun () -> parse "place   1     d    d"));

  "remove1" >:: (fun _ -> assert_equal (Remove ["u"; "12321"; "1212"]) 
                    (parse "remove   u 12321            1212"));
  "remove2" >:: (fun _ -> assert_equal (Remove ["a"; "2"; "3"]) 
                    (parse "reMoVE            a 2 3"));
  "remove3" >:: (fun _ -> assert_equal (Remove ["i"; "3"; "10"]) 
                    (parse "REMOVE i          3               10"));
  "remove4" >:: (fun _ -> assert_equal (Remove ["r"; "100"; "132"]) 
                    (parse "remove r 100            132"));
  "remove5" >:: (fun _ -> assert_equal (Remove ["x"; "11"; "12"]) 
                    (parse "remove       x 11            12"));
  "remove6" >:: (fun _ -> assert_equal (Remove ["u"; "12321"; "1212"]) 
                    (parse "remove   u 12321            1212"));
  "remove7" >:: (fun _ -> assert_equal (Remove ["u"; "39"; "12"]) 
                    (parse "remove u 39 12"));

  "init1" >:: (fun _ -> assert_equal  (Init ["1";"1"])   (parse "init 1 1"));
  "init2" >:: (fun _ -> assert_equal  (Init ["1";"1"])   (parse "  inIT   1 1   "));
  "init3" >:: (fun _ -> assert_equal  (Init ["35";"24"]) (parse "INIT 35 24"));
  "init4" >:: (fun _ -> assert_raises (Malformed) (fun () -> parse "init s a"));
  "init5" >:: (fun _ -> assert_raises (Malformed) (fun () -> parse "init rewr"));
  "init6" >:: (fun _ -> assert_raises (Malformed) (fun () -> parse "iNiT 34"));
  
  "play1" >:: (fun _ -> assert_raises (Malformed) (fun () -> parse "play 35 24"));
  "play2" >:: (fun _ -> assert_raises (Malformed) (fun () -> parse "play 35"));
  "play3" >:: (fun _ -> assert_raises (Malformed) (fun () -> parse "play nhjs"));
  "play4" >:: (fun _ -> assert_equal  (Play) (parse "play"));
  "play5" >:: (fun _ -> assert_equal  (Play) (parse "PLAY"));
  "play6" >:: (fun _ -> assert_equal  (Play) (parse "PLay"));

  "recall1" >:: (fun _ -> assert_equal  (Recall) (parse "RECALL"));
  "recall2" >:: (fun _ -> assert_equal  (Recall) (parse "REcaLL"));
  "recall3" >:: (fun _ -> assert_raises (Malformed) (fun () -> parse "REcaLL djsal d"));
  "recall4" >:: (fun _ -> assert_raises (Malformed) (fun () -> parse "REcaLL 1"));
  "recall5" >:: (fun _ -> assert_raises (Malformed) (fun () -> parse "recall s s "));

  "discard1" >:: (fun _ -> assert_equal  (Discard["a"]) (parse "discard a"));
  "discard2" >:: (fun _ -> assert_equal  (Discard["sad"]) (parse "diSCard SAD"));
  "discard3" >:: (fun _ -> assert_equal  (Discard["sad"; "face"; "dfskja"]) 
                     (parse "diSCard SAD FaCE dfskja"));
  "discard4" >:: (fun _ -> assert_raises (Malformed)
                     (fun () -> parse "dis  dsfkhljkahs card"));
  "discard5" >:: (fun _ -> assert_raises (Malformed) 
                     (fun () -> parse "D      ISC    ARD"));
  "discard6" >:: (fun _ -> assert_raises (Malformed) 
                     (fun () -> parse "           dIScArD     "));

  "quit1" >:: (fun _ -> assert_equal  (Quit) (parse "   quit   "));
  "quit2" >:: (fun _ -> assert_equal  (Quit) (parse "  QUIT"));
  "quit3" >:: (fun _ -> assert_raises (Malformed) (fun () -> parse "  QU  I   T"));
  "quit4" >:: (fun _ -> assert_raises (Malformed) (fun () -> parse "quit djsal d"));
  "quit5" >:: (fun _ -> assert_raises (Malformed) (fun () -> parse "qreca dj # d"));

  "shuffle1" >:: (fun _ -> assert_equal  (Shuffle) (parse "   SHUFFLE   "));
  "shuffle2" >:: (fun _ -> assert_equal  (Shuffle) (parse "shufflE   "));
  "shuffle3" >:: (fun _ -> assert_raises (Malformed) (fun () -> parse " s f fehn,jkdh"));
  "shuffle4" >:: (fun _ -> assert_raises (Malformed) (fun () -> parse " s shuffle"));
  "shuffle5" >:: (fun _ -> assert_raises (Malformed) (fun () -> parse " SHUFFLE ABC"));

  "help1" >:: (fun _ -> assert_equal  (Help) (parse "help   "));
  "help2" >:: (fun _ -> assert_equal  (Help) (parse "   HeLP   "));
  "help3" >:: (fun _ -> assert_raises (Malformed) (fun () -> parse " help s 3 "));
  "help4" >:: (fun _ -> assert_raises (Malformed) (fun () -> parse " help 1 3"));
  "help5" >:: (fun _ -> assert_raises (Malformed) (fun () -> parse " 1 HELP 0 "));

  "okay1" >:: (fun _ -> assert_equal  (Okay) (parse " okaY     "));
  "okay2" >:: (fun _ -> assert_raises (Malformed) (fun () -> parse "okay 1 0 dfkf"));
]

let init_state_result = init_state 10 10 

let extract_state state =
  match state with
  | Valid st -> st
  | Invalid -> raise IllegalMove

let extracted_init_state = extract_state init_state_result
let init_player_tiles    = extracted_init_state.player_tiles
let init_opponent_tiles  = extracted_init_state.opponent_tiles
let init_opponent_score  = extracted_init_state.opponent_score
let init_player_score    = extracted_init_state.player_score
let init_board           = extracted_init_state.board
let init_midtiles        = extracted_init_state.midtiles
let init_prev_play       = extracted_init_state.prev_play

let init_state_tests = [
  "init_tiles"    >:: (fun _ -> assert_equal 8 (List.length init_player_tiles));
  "init_tiles"    >:: (fun _ -> assert_equal 8 (List.length init_opponent_tiles));
  "init_score"    >:: (fun _ -> assert_equal 0 init_opponent_score);
  "init_score"    >:: (fun _ -> assert_equal 0 init_player_score);
  "init_board"    >:: (fun _ -> assert_equal 10 (Array.length init_board.(0)));
  "init_board"    >:: (fun _ -> assert_equal 10 (Array.length init_board));
  "init_midtiles" >:: (fun _ -> assert_equal 0 (List.length init_midtiles));
  "init_prev"     >:: (fun _ -> assert_equal 0 (List.length init_prev_play));
]

let a_tile = Char.escaped (List.hd init_player_tiles)
let basic_state = tile_state [] extracted_init_state 2
let extracted_basic_state = extract_state basic_state
let basic_player_tiles    = extracted_basic_state.player_tiles
let basic_opponent_tiles  = extracted_basic_state.opponent_tiles
let basic_opponent_score  = extracted_basic_state.opponent_score
let basic_player_score    = extracted_basic_state.player_score
let basic_board           = extracted_basic_state.board
let basic_midtiles        = extracted_basic_state.midtiles
let basic_prev_play       = extracted_basic_state.prev_play

let basic_state_action_tests = [
  "basic_tiles"    >:: (fun _ -> assert_equal 8 (List.length basic_player_tiles));
  "basic_tiles"    >:: (fun _ -> assert_equal 8 (List.length basic_opponent_tiles));
  "basic_score"    >:: (fun _ -> assert_equal 0 basic_opponent_score);
  "basic_score"    >:: (fun _ -> assert_equal 0 basic_player_score);
  "basic_board"    >:: (fun _ -> assert_equal 10 (Array.length basic_board.(0)));
  "basic_board"    >:: (fun _ -> assert_equal 10 (Array.length basic_board));
  "basic_midtiles" >:: (fun _ -> assert_equal 0 (List.length basic_midtiles));
  "basic_prev"     >:: (fun _ -> assert_equal 0 (List.length basic_prev_play));
]

let tests =
  "test suite for A7"  >::: List.flatten [
    command_tests;
    init_state_tests;
    basic_state_action_tests;
  ]

let _ = run_test_tt_main tests
