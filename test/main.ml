(** Test plan

Our OUnit tests thoroughly test the functionality of the Board and State modules. 
We first explicitly test edge cases for board and state functions, such as 
checking move order, castling ability, and moving pieces with occluding pieces 
(ex. if a pawn is moved forward 2 squares but there is a piece one square ahead 
of the pawn, it is an invalid move.) Then we have a large number of tests that 
are just random actual chess games, which sample a variety of possibilities. 
We check that each move results in a valid board and that the piece we move 
is in the correct position after the move. Since the games play out fully 
without any invalid moves, we have reasonable confidence that no valid moves are
marked invalid, and our edge cases test that no invalid moves are marked valid.
Many of the explicit edge cases were developed using glass-box testing, whereas
the tests at the end are black-box tests developed from random actual chess 
games. The reason we chose random chess games for tests is so we could test
the game in a variety of states, such as a late-game board. If we just randomly
generated moves, we would explore a smaller space of the countless possibilities 
for a chess game. We manually tested our Command module, main.ml, and board 
printing functions by playing our game through its terminal interface. 
*)

open OUnit2
open Chessmon
open State
open Board

let string_of_list ?(open_delim = "[") ?(close_delim = "]") ?(sep = "; ")
    string_of_elt lst =
  let len = List.length lst in
  let open Buffer in
  (* As a rough lower bound assume that each element takes a minimum of 3
     characters to represent including a separator, e.g., ["v, "]. The buffer
     will grow as needed, so it's okay if that estimate is low. *)
  let buf = create (3 * len) in
  add_string buf open_delim;
  List.iteri
    (fun i v ->
      add_string buf (string_of_elt v);
      if i < len - 1 then add_string buf sep)
    lst;
  add_string buf close_delim;
  contents buf

let to_string_pair (p : char * int) : string =
  "(" ^ (fst p |> Char.escaped) ^ ", " ^ (snd p |> Int.to_string) ^ ")"

(** [move_test name state start_col start_row end_col end_row expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_piece_type] with
    [(Board.get_piece (move state start_col start_row end_col end_row |> Option.get).board at_col at_row |> Option.get).piece_type]. *)
let move_test (name : string) (state : state) (start_col : char)
    (start_row : int) (end_col : char) (end_row : int)
    (expected_piece_type : Board.piece_type) (at_col : char) (at_row : int) =
  name >:: fun _ ->
  assert_equal expected_piece_type
    (Option.get
       (Board.get_piece
          (State.move start_col start_row end_col end_row state |> Option.get)
            .board at_col at_row))
      .piece_type

(** [invalid_move_test name state start_col start_row end_col end_row] constructs an OUnit test named
    [name] that asserts the quality of [None] with
    [move state start_col start_row end_col end_row]. *)
let invalid_move_test (name : string) (state : state) (start_col : char)
    (start_row : int) (end_col : char) (end_row : int) =
  name >:: fun _ ->
  assert_equal None (State.move start_col start_row end_col end_row state)

let castle_state_white =
  State.init_state |> State.move 'E' 2 'E' 4 |> Option.get
  |> State.move 'E' 7 'E' 6 |> Option.get |> State.move 'F' 1 'E' 2
  |> Option.get |> State.move 'F' 8 'E' 7 |> Option.get
  |> State.move 'G' 1 'H' 3 |> Option.get |> State.move 'G' 8 'H' 6
  |> Option.get |> State.move 'D' 2 'D' 4 |> Option.get
  |> State.move 'D' 7 'D' 5 |> Option.get |> State.move 'D' 1 'D' 3
  |> Option.get |> State.move 'D' 8 'D' 6 |> Option.get
  |> State.move 'C' 1 'D' 2 |> Option.get |> State.move 'C' 8 'D' 7
  |> Option.get |> State.move 'B' 1 'A' 3 |> Option.get
  |> State.move 'B' 8 'A' 6 |> Option.get

(* White's turn
   8 R . . . K . . R
   7 P P P B B P P P
   6 N . . Q P . . N
   5 . . . P . . . .
   4 . . . P P . . .
   3 N . . Q . . . N
   2 P P P B B P P P
   1 R . . . K . . R
     A B C D E F G H
*)

let castle_state_black =
  castle_state_white |> State.move 'B' 2 'B' 3 |> Option.get
(* Black's turn
   8 R . . . K . . R
   7 P P P B B P P P
   6 N . . Q P . . N
   5 . . . P . . . .
   4 . . . P P . . .
   3 N P . Q . . . N
   2 P . P B B P P P
   1 R . . . K . . R
     A B C D E F G H
*)

let neither_can_castle =
  castle_state_white |> State.move 'E' 1 'F' 1 |> Option.get
  |> State.move 'E' 8 'F' 8 |> Option.get |> State.move 'F' 1 'E' 1
  |> Option.get |> State.move 'F' 8 'E' 8 |> Option.get

let weird_board_test =
  State.init_state |> State.move 'E' 2 'E' 4 |> Option.get
  |> State.move 'F' 7 'F' 5 |> Option.get

let (test_pawn : Board.piece) =
  { piece_type = Pawn; color = Black; column = 'F'; row = 5 }

(** TODO: test that moving rook prohibits castling one way *)
let state_tests =
  [
    invalid_move_test "black can't move first" State.init_state 'A' 7 'A' 6;
    invalid_move_test "after white moves, black must move"
      (State.init_state |> State.move 'E' 2 'E' 4 |> Option.get)
      'A' 2 'A' 3;
    move_test "castle right white, king pos" castle_state_white 'E' 1 'G' 1 King
      'G' 1;
    move_test "castle right white, rook pos" castle_state_white 'E' 1 'G' 1 Rook
      'F' 1;
    move_test "castle left white, king pos" castle_state_white 'E' 1 'C' 1 King
      'C' 1;
    move_test "castle left white, rook pos" castle_state_white 'E' 1 'C' 1 Rook
      'D' 1;
    move_test "castle right black, king pos" castle_state_black 'E' 8 'G' 8 King
      'G' 8;
    move_test "castle right black, rook pos" castle_state_black 'E' 8 'G' 8 Rook
      'F' 8;
    move_test "castle left black, king pos" castle_state_black 'E' 8 'C' 8 King
      'C' 8;
    move_test "castle left black, rook pos" castle_state_black 'E' 8 'C' 8 Rook
      'D' 8;
    ( "white can't castle right after king move" >:: fun _ ->
      assert_equal false neither_can_castle.white_state.can_castle_right );
    ( "white can't castle left after king move" >:: fun _ ->
      assert_equal false neither_can_castle.white_state.can_castle_left );
    ( "black can't castle right after king move" >:: fun _ ->
      assert_equal false neither_can_castle.black_state.can_castle_right );
    ( "black can't castle left after king move" >:: fun _ ->
      assert_equal false neither_can_castle.black_state.can_castle_left );
  ]

exception EmptyBoard
exception EmptyPiece

let extract_board (bopt : Board.board option) =
  match bopt with Some b -> b | None -> raise EmptyBoard

let extract_piece (popt : Board.piece option) =
  match popt with Some p -> p | None -> raise EmptyPiece

let extract_state (sopt : State.state option) = Option.get sopt

let check_if_occupied_test (name : string) (board : Board.board) (c : char)
    (i : int) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (Board.check_if_occupied board c i)

let check_btwn_squares_test (name : string) (board : Board.board)
    (p : Board.piece) (c : char) (i : int) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (Board.check_btwn_squares board p c i)

let check_each_square_test (name : string) (board : Board.board)
    (lst : (char * int) list) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (Board.check_each_square board lst)

let find_path_test (name : string) (piece : Board.piece) (board : Board.board)
    ((start_col, start_row) : char * int) ((end_col, end_row) : char * int)
    (expected_output : (char * int) list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.find_path board piece (start_col, start_row) (end_col, end_row))
    ~printer:(string_of_list to_string_pair)

let check_valid_move_test (name : string) board (piece : Board.piece) (c : char)
    (i : int) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.check_valid_move board piece c i)
    ~printer:string_of_bool

let board_move_test (name : string) (board : Board.board) (c1 : char) (i1 : int)
    (c2 : char) (i2 : int) (can_castle_left : bool) (can_castle_right : bool)
    (expected_output : Board.board option) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Board.move board c1 i2 c2 i2 can_castle_left can_castle_right)

let check_bishop_end_pos_test (name : string) (piece : Board.piece) (c : char)
    (i : int) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (Board.check_bishop_end_pos piece c i)

let check_rook_end_pos_test (name : string) (piece : Board.piece) (c : char)
    (i : int) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (Board.check_rook_end_pos piece c i)

let board_tests =
  [
    check_if_occupied_test "starting position" Board.init_board 'A' 1 true;
    check_if_occupied_test "after moving a piece"
      Board.(move init_board 'D' 2 'D' 3 false false |> extract_board)
      'D' 3 true;
    check_if_occupied_test "check original spot after moving the piece"
      Board.(move init_board 'D' 2 'D' 3 false false |> extract_board)
      'D' 2 false;
    check_btwn_squares_test "check able to move pawns at beginning"
      Board.init_board
      { piece_type = Pawn; color = White; column = 'E'; row = 2 }
      'E' 4 true;
    check_btwn_squares_test
      "returns false if piece in between start and end square"
      Board.(
        move
          (move init_board 'E' 2 'E' 4 false false |> extract_board)
          'D' 1 'F' 3 false false
        |> extract_board)
      { piece_type = Pawn; color = White; column = 'F'; row = 2 }
      'F' 4 false;
    check_each_square_test "returns false if a square is occupied"
      Board.init_board
      [ ('D', 4); ('H', 6); ('C', 8) ]
      false;
    check_each_square_test "returns true if no square is occupied"
      Board.init_board
      [ ('A', 3); ('D', 4); ('F', 5); ('H', 6) ]
      true;
    find_path_test "should return one square for pawn"
      { piece_type = Pawn; color = White; column = 'B'; row = 2 }
      Board.init_board ('B', 3) ('B', 4)
      [ ('B', 3) ];
    check_valid_move_test "returns false if square is occupied - pawn"
      Board.(
        move
          (move init_board 'E' 2 'E' 4 false false |> extract_board)
          'D' 1 'F' 3 false false
        |> extract_board)
      { piece_type = Pawn; color = White; column = 'F'; row = 2 }
      'F' 4 false;
    board_move_test "returns None if invalid move"
      Board.(
        move
          (move init_board 'E' 2 'E' 4 false false |> extract_board)
          'D' 1 'F' 3 false false
        |> extract_board)
      'F' 2 'F' 4 false false None;
    check_bishop_end_pos_test "returns true if valid end pos"
      { piece_type = Bishop; color = Black; column = 'D'; row = 5 }
      'G' 2 true;
    check_bishop_end_pos_test "returns false if not valid end pos"
      { piece_type = Bishop; color = Black; column = 'D'; row = 5 }
      'A' 7 false;
    check_valid_move_test "returns true if no square occupied - bishop"
      Board.(move init_board 'G' 2 'G' 4 false false |> extract_board)
      { piece_type = Bishop; color = White; column = 'F'; row = 1 }
      'H' 3 true;
    check_valid_move_test "returns false if square occupied - bishop"
      Board.(
        move
          (move init_board 'E' 2 'E' 4 false false |> extract_board)
          'D' 2 'D' 3 false false
        |> extract_board)
      { piece_type = Bishop; color = White; column = 'F'; row = 1 }
      'B' 7 false;
    check_rook_end_pos_test "returns true if valid end pos"
      { piece_type = Rook; color = White; column = 'F'; row = 7 }
      'F' 1 true;
    check_rook_end_pos_test "returns false if not valid end pos"
      { piece_type = Rook; color = Black; column = 'A'; row = 1 }
      'H' 8 false;
    check_valid_move_test "returns true if no square occupied - rook"
      Board.(move init_board 'H' 2 'H' 4 false false |> extract_board)
      { piece_type = Rook; color = White; column = 'H'; row = 1 }
      'H' 3 true;
    find_path_test "should return one square for pawn"
      { piece_type = Rook; color = White; column = 'H'; row = 1 }
      Board.(move init_board 'H' 2 'H' 4 false false |> extract_board)
      ('H', 2) ('H', 6)
      [ ('H', 2); ('H', 3); ('H', 4); ('H', 5) ];
    check_each_square_test "returns false if a square is occupied"
      Board.(move init_board 'H' 2 'H' 4 false false |> extract_board)
      [ ('H', 2); ('H', 3); ('H', 4); ('H', 5) ]
      false;
    check_valid_move_test "returns false if square occupied - rook"
      Board.(move init_board 'H' 2 'H' 4 false false |> extract_board)
      { piece_type = Rook; color = White; column = 'H'; row = 1 }
      'H' 6 false;
    check_valid_move_test "testing queen rows"
      Board.(
        move
          (move
             (move init_board 'D' 2 'D' 4 false false |> extract_board)
             'C' 2 'C' 4 false false
          |> extract_board)
          'D' 1 'D' 2 false false
        |> extract_board)
      { piece_type = Queen; color = White; column = 'D'; row = 2 }
      'G' 2 false;
    check_valid_move_test "testing queen columns"
      Board.(move init_board 'D' 2 'D' 4 false false |> extract_board)
      { piece_type = Queen; color = White; column = 'D'; row = 1 }
      'D' 6 false;
    check_valid_move_test "testing queen diagonals" Board.init_board
      { piece_type = Queen; color = Black; column = 'D'; row = 8 }
      'H' 4 false;
    check_valid_move_test "testing queen diagonals - true"
      Board.(move init_board 'E' 7 'E' 5 false false |> extract_board)
      { piece_type = Queen; color = Black; column = 'D'; row = 8 }
      'H' 4 true;
    ( "is the black pawn there" >:: fun _ ->
      assert_equal (Some test_pawn)
        (Board.get_piece weird_board_test.board 'F' 5) );
    check_valid_move_test "knight shouldn't be able to move all across board"
      Board.init_board
      { piece_type = Knight; color = Black; column = 'G'; row = 8 }
      'E' 1 false;
    check_valid_move_test "knight shouldn't be able to move all across board"
      Board.init_board
      { piece_type = Knight; color = Black; column = 'B'; row = 8 }
      'D' 1 false;
  ]

let test_0_state = State.init_state
let test_1_state = test_0_state |> State.move 'E' 2 'E' 4 |> Option.get
let test_2_state = test_1_state |> State.move 'E' 7 'E' 5 |> Option.get
let test_3_state = test_2_state |> State.move 'D' 2 'D' 4 |> Option.get
let test_4_state = test_3_state |> State.move 'B' 8 'C' 6 |> Option.get
let test_5_state = test_4_state |> State.move 'D' 4 'E' 5 |> Option.get
let test_6_state = test_5_state |> State.move 'C' 6 'E' 5 |> Option.get
let test_7_state = test_6_state |> State.move 'G' 1 'F' 3 |> Option.get
let test_8_state = test_7_state |> State.move 'E' 5 'F' 3 |> Option.get
let test_9_state = test_8_state |> State.move 'D' 1 'F' 3 |> Option.get
let test_10_state = test_9_state |> State.move 'D' 8 'F' 6 |> Option.get
let test_11_state = test_10_state |> State.move 'B' 1 'C' 3 |> Option.get
let test_12_state = test_11_state |> State.move 'C' 7 'C' 6 |> Option.get
let test_13_state = test_12_state |> State.move 'C' 1 'F' 4 |> Option.get
let test_14_state = test_13_state |> State.move 'D' 7 'D' 6 |> Option.get
let test_15_state = test_14_state |> State.move 'A' 1 'D' 1 |> Option.get
let test_16_state = test_15_state |> State.move 'F' 6 'E' 6 |> Option.get
let test_17_state = test_16_state |> State.move 'F' 3 'D' 3 |> Option.get
let test_18_state = test_17_state |> State.move 'G' 8 'F' 6 |> Option.get
let test_19_state = test_18_state |> State.move 'F' 1 'E' 2 |> Option.get
let test_20_state = test_19_state |> State.move 'F' 8 'E' 7 |> Option.get
let test_21_state = test_20_state |> State.move 'F' 4 'D' 6 |> Option.get
let test_22_state = test_21_state |> State.move 'E' 7 'D' 6 |> Option.get
let test_23_state = test_22_state |> State.move 'D' 3 'D' 6 |> Option.get
let test_24_state = test_23_state |> State.move 'E' 6 'D' 6 |> Option.get
let test_25_state = test_24_state |> State.move 'D' 1 'D' 6 |> Option.get
let test_26_state = test_25_state |> State.move 'E' 8 'E' 7 |> Option.get
let test_27_state = test_26_state |> State.move 'E' 4 'E' 5 |> Option.get
let test_28_state = test_27_state |> State.move 'F' 6 'D' 7 |> Option.get
let test_29_state = test_28_state |> State.move 'F' 2 'F' 4 |> Option.get
let test_30_state = test_29_state |> State.move 'F' 7 'F' 6 |> Option.get
let test_31_state = test_30_state |> State.move 'E' 1 'G' 1 |> Option.get
let test_32_state = test_31_state |> State.move 'F' 6 'E' 5 |> Option.get
let test_33_state = test_32_state |> State.move 'F' 4 'E' 5 |> Option.get
let test_34_state = test_33_state |> State.move 'D' 7 'E' 5 |> Option.get
let test_35_state = test_34_state |> State.move 'C' 3 'E' 4 |> Option.get
let test_36_state = test_35_state |> State.move 'C' 8 'E' 6 |> Option.get
let test_37_state = test_36_state |> State.move 'A' 2 'A' 4 |> Option.get
let test_38_state = test_37_state |> State.move 'E' 6 'D' 5 |> Option.get
let test_39_state = test_38_state |> State.move 'D' 6 'D' 5 |> Option.get
let test_40_state = test_39_state |> State.move 'C' 6 'D' 5 |> Option.get
let test_41_state = test_40_state |> State.move 'E' 4 'C' 3 |> Option.get
let test_42_state = test_41_state |> State.move 'A' 8 'D' 8 |> Option.get
let test_43_state = test_42_state |> State.move 'F' 1 'D' 1 |> Option.get
let test_44_state = test_43_state |> State.move 'D' 5 'D' 4 |> Option.get
let test_45_state = test_44_state |> State.move 'C' 3 'B' 5 |> Option.get
let test_46_state = test_45_state |> State.move 'E' 5 'C' 6 |> Option.get
let test_47_state = test_46_state |> State.move 'D' 1 'E' 1 |> Option.get
let test_48_state = test_47_state |> State.move 'E' 7 'F' 6 |> Option.get
let test_49_state = test_48_state |> State.move 'E' 1 'F' 1 |> Option.get
let test_50_state = test_49_state |> State.move 'F' 6 'E' 7 |> Option.get
let test_51_state = test_50_state |> State.move 'F' 1 'E' 1 |> Option.get
let test_52_state = test_51_state |> State.move 'E' 7 'F' 6 |> Option.get
let test_53_state = test_52_state |> State.move 'E' 1 'F' 1 |> Option.get
let test_54_state = test_53_state |> State.move 'F' 6 'E' 7 |> Option.get

let random_game_1_tests =
  [
    move_test "generated test 0 from actual game" test_0_state 'E' 2 'E' 4 Pawn
      'E' 4;
    move_test "generated test 1 from actual game" test_1_state 'E' 7 'E' 5 Pawn
      'E' 5;
    move_test "generated test 2 from actual game" test_2_state 'D' 2 'D' 4 Pawn
      'D' 4;
    move_test "generated test 3 from actual game" test_3_state 'B' 8 'C' 6
      Knight 'C' 6;
    move_test "generated test 4 from actual game" test_4_state 'D' 4 'E' 5 Pawn
      'E' 5;
    move_test "generated test 5 from actual game" test_5_state 'C' 6 'E' 5
      Knight 'E' 5;
    move_test "generated test 6 from actual game" test_6_state 'G' 1 'F' 3
      Knight 'F' 3;
    move_test "generated test 7 from actual game" test_7_state 'E' 5 'F' 3
      Knight 'F' 3;
    move_test "generated test 8 from actual game" test_8_state 'D' 1 'F' 3 Queen
      'F' 3;
    move_test "generated test 9 from actual game" test_9_state 'D' 8 'F' 6 Queen
      'F' 6;
    move_test "generated test 10 from actual game" test_10_state 'B' 1 'C' 3
      Knight 'C' 3;
    move_test "generated test 11 from actual game" test_11_state 'C' 7 'C' 6
      Pawn 'C' 6;
    move_test "generated test 12 from actual game" test_12_state 'C' 1 'F' 4
      Bishop 'F' 4;
    move_test "generated test 13 from actual game" test_13_state 'D' 7 'D' 6
      Pawn 'D' 6;
    move_test "generated test 14 from actual game" test_14_state 'A' 1 'D' 1
      Rook 'D' 1;
    move_test "generated test 15 from actual game" test_15_state 'F' 6 'E' 6
      Queen 'E' 6;
    move_test "generated test 16 from actual game" test_16_state 'F' 3 'D' 3
      Queen 'D' 3;
    move_test "generated test 17 from actual game" test_17_state 'G' 8 'F' 6
      Knight 'F' 6;
    move_test "generated test 18 from actual game" test_18_state 'F' 1 'E' 2
      Bishop 'E' 2;
    move_test "generated test 19 from actual game" test_19_state 'F' 8 'E' 7
      Bishop 'E' 7;
    move_test "generated test 20 from actual game" test_20_state 'F' 4 'D' 6
      Bishop 'D' 6;
    move_test "generated test 21 from actual game" test_21_state 'E' 7 'D' 6
      Bishop 'D' 6;
    move_test "generated test 22 from actual game" test_22_state 'D' 3 'D' 6
      Queen 'D' 6;
    move_test "generated test 23 from actual game" test_23_state 'E' 6 'D' 6
      Queen 'D' 6;
    move_test "generated test 24 from actual game" test_24_state 'D' 1 'D' 6
      Rook 'D' 6;
    move_test "generated test 25 from actual game" test_25_state 'E' 8 'E' 7
      King 'E' 7;
    move_test "generated test 26 from actual game" test_26_state 'E' 4 'E' 5
      Pawn 'E' 5;
    move_test "generated test 27 from actual game" test_27_state 'F' 6 'D' 7
      Knight 'D' 7;
    move_test "generated test 28 from actual game" test_28_state 'F' 2 'F' 4
      Pawn 'F' 4;
    move_test "generated test 29 from actual game" test_29_state 'F' 7 'F' 6
      Pawn 'F' 6;
    move_test "generated test 30 from actual game" test_30_state 'E' 1 'G' 1
      King 'G' 1;
    move_test "generated test 31 from actual game" test_31_state 'F' 6 'E' 5
      Pawn 'E' 5;
    move_test "generated test 32 from actual game" test_32_state 'F' 4 'E' 5
      Pawn 'E' 5;
    move_test "generated test 33 from actual game" test_33_state 'D' 7 'E' 5
      Knight 'E' 5;
    move_test "generated test 34 from actual game" test_34_state 'C' 3 'E' 4
      Knight 'E' 4;
    move_test "generated test 35 from actual game" test_35_state 'C' 8 'E' 6
      Bishop 'E' 6;
    move_test "generated test 36 from actual game" test_36_state 'A' 2 'A' 4
      Pawn 'A' 4;
    move_test "generated test 37 from actual game" test_37_state 'E' 6 'D' 5
      Bishop 'D' 5;
    move_test "generated test 38 from actual game" test_38_state 'D' 6 'D' 5
      Rook 'D' 5;
    move_test "generated test 39 from actual game" test_39_state 'C' 6 'D' 5
      Pawn 'D' 5;
    move_test "generated test 40 from actual game" test_40_state 'E' 4 'C' 3
      Knight 'C' 3;
    move_test "generated test 41 from actual game" test_41_state 'A' 8 'D' 8
      Rook 'D' 8;
    move_test "generated test 42 from actual game" test_42_state 'F' 1 'D' 1
      Rook 'D' 1;
    move_test "generated test 43 from actual game" test_43_state 'D' 5 'D' 4
      Pawn 'D' 4;
    move_test "generated test 44 from actual game" test_44_state 'C' 3 'B' 5
      Knight 'B' 5;
    move_test "generated test 45 from actual game" test_45_state 'E' 5 'C' 6
      Knight 'C' 6;
    move_test "generated test 46 from actual game" test_46_state 'D' 1 'E' 1
      Rook 'E' 1;
    move_test "generated test 47 from actual game" test_47_state 'E' 7 'F' 6
      King 'F' 6;
    move_test "generated test 48 from actual game" test_48_state 'E' 1 'F' 1
      Rook 'F' 1;
    move_test "generated test 49 from actual game" test_49_state 'F' 6 'E' 7
      King 'E' 7;
    move_test "generated test 50 from actual game" test_50_state 'F' 1 'E' 1
      Rook 'E' 1;
    move_test "generated test 51 from actual game" test_51_state 'E' 7 'F' 6
      King 'F' 6;
    move_test "generated test 52 from actual game" test_52_state 'E' 1 'F' 1
      Rook 'F' 1;
    move_test "generated test 53 from actual game" test_53_state 'F' 6 'E' 7
      King 'E' 7;
    move_test "generated test 54 from actual game" test_54_state 'F' 1 'E' 1
      Rook 'E' 1;
  ]

let test_0_state = State.init_state
let test_1_state = test_0_state |> State.move 'E' 2 'E' 4 |> Option.get
let test_2_state = test_1_state |> State.move 'E' 7 'E' 5 |> Option.get
let test_3_state = test_2_state |> State.move 'G' 1 'F' 3 |> Option.get
let test_4_state = test_3_state |> State.move 'G' 8 'F' 6 |> Option.get
let test_5_state = test_4_state |> State.move 'F' 3 'E' 5 |> Option.get
let test_6_state = test_5_state |> State.move 'D' 7 'D' 6 |> Option.get
let test_7_state = test_6_state |> State.move 'E' 5 'F' 3 |> Option.get
let test_8_state = test_7_state |> State.move 'F' 6 'E' 4 |> Option.get
let test_9_state = test_8_state |> State.move 'B' 1 'C' 3 |> Option.get
let test_10_state = test_9_state |> State.move 'E' 4 'C' 3 |> Option.get
let test_11_state = test_10_state |> State.move 'D' 2 'C' 3 |> Option.get
let test_12_state = test_11_state |> State.move 'C' 8 'G' 4 |> Option.get
let test_13_state = test_12_state |> State.move 'H' 2 'H' 3 |> Option.get
let test_14_state = test_13_state |> State.move 'G' 4 'F' 3 |> Option.get
let test_15_state = test_14_state |> State.move 'D' 1 'F' 3 |> Option.get
let test_16_state = test_15_state |> State.move 'D' 8 'E' 7 |> Option.get
let test_17_state = test_16_state |> State.move 'C' 1 'E' 3 |> Option.get
let test_18_state = test_17_state |> State.move 'B' 8 'C' 6 |> Option.get
let test_19_state = test_18_state |> State.move 'F' 1 'B' 5 |> Option.get
let test_20_state = test_19_state |> State.move 'E' 8 'D' 7 |> Option.get
let test_21_state = test_20_state |> State.move 'E' 1 'C' 1 |> Option.get
let test_22_state = test_21_state |> State.move 'E' 7 'E' 6 |> Option.get
let test_23_state = test_22_state |> State.move 'H' 1 'E' 1 |> Option.get
let test_24_state = test_23_state |> State.move 'E' 6 'A' 2 |> Option.get
let test_25_state = test_24_state |> State.move 'E' 3 'G' 5 |> Option.get
let test_26_state = test_25_state |> State.move 'F' 7 'F' 6 |> Option.get
let test_27_state = test_26_state |> State.move 'F' 3 'G' 4 |> Option.get
let test_28_state = test_27_state |> State.move 'D' 7 'D' 8 |> Option.get
let test_29_state = test_28_state |> State.move 'B' 5 'C' 6 |> Option.get
let test_30_state = test_29_state |> State.move 'B' 7 'C' 6 |> Option.get
let test_31_state = test_30_state |> State.move 'G' 4 'E' 4 |> Option.get
let test_32_state = test_31_state |> State.move 'A' 2 'A' 1 |> Option.get
let test_33_state = test_32_state |> State.move 'C' 1 'D' 2 |> Option.get
let test_34_state = test_33_state |> State.move 'D' 8 'C' 8 |> Option.get
let test_35_state = test_34_state |> State.move 'E' 4 'C' 6 |> Option.get
let test_36_state = test_35_state |> State.move 'A' 1 'D' 1 |> Option.get
let test_37_state = test_36_state |> State.move 'D' 2 'D' 1 |> Option.get
let test_38_state = test_37_state |> State.move 'F' 8 'E' 7 |> Option.get
let test_39_state = test_38_state |> State.move 'E' 1 'E' 7 |> Option.get
let test_40_state = test_39_state |> State.move 'C' 8 'B' 8 |> Option.get

let random_game_2_tests =
  [
    move_test "generated test 0 from actual game 2" test_0_state 'E' 2 'E' 4
      Pawn 'E' 4;
    move_test "generated test 1 from actual game 2" test_1_state 'E' 7 'E' 5
      Pawn 'E' 5;
    move_test "generated test 2 from actual game 2" test_2_state 'G' 1 'F' 3
      Knight 'F' 3;
    move_test "generated test 3 from actual game 2" test_3_state 'G' 8 'F' 6
      Knight 'F' 6;
    move_test "generated test 4 from actual game 2" test_4_state 'F' 3 'E' 5
      Knight 'E' 5;
    move_test "generated test 5 from actual game 2" test_5_state 'D' 7 'D' 6
      Pawn 'D' 6;
    move_test "generated test 6 from actual game 2" test_6_state 'E' 5 'F' 3
      Knight 'F' 3;
    move_test "generated test 7 from actual game 2" test_7_state 'F' 6 'E' 4
      Knight 'E' 4;
    move_test "generated test 8 from actual game 2" test_8_state 'B' 1 'C' 3
      Knight 'C' 3;
    move_test "generated test 9 from actual game 2" test_9_state 'E' 4 'C' 3
      Knight 'C' 3;
    move_test "generated test 10 from actual game 2" test_10_state 'D' 2 'C' 3
      Pawn 'C' 3;
    move_test "generated test 11 from actual game 2" test_11_state 'C' 8 'G' 4
      Bishop 'G' 4;
    move_test "generated test 12 from actual game 2" test_12_state 'H' 2 'H' 3
      Pawn 'H' 3;
    move_test "generated test 13 from actual game 2" test_13_state 'G' 4 'F' 3
      Bishop 'F' 3;
    move_test "generated test 14 from actual game 2" test_14_state 'D' 1 'F' 3
      Queen 'F' 3;
    move_test "generated test 15 from actual game 2" test_15_state 'D' 8 'E' 7
      Queen 'E' 7;
    move_test "generated test 16 from actual game 2" test_16_state 'C' 1 'E' 3
      Bishop 'E' 3;
    move_test "generated test 17 from actual game 2" test_17_state 'B' 8 'C' 6
      Knight 'C' 6;
    move_test "generated test 18 from actual game 2" test_18_state 'F' 1 'B' 5
      Bishop 'B' 5;
    move_test "generated test 19 from actual game 2" test_19_state 'E' 8 'D' 7
      King 'D' 7;
    move_test "generated test 20 from actual game 2" test_20_state 'E' 1 'C' 1
      King 'C' 1;
    move_test "generated test 21 from actual game 2" test_21_state 'E' 7 'E' 6
      Queen 'E' 6;
    move_test "generated test 22 from actual game 2" test_22_state 'H' 1 'E' 1
      Rook 'E' 1;
    move_test "generated test 23 from actual game 2" test_23_state 'E' 6 'A' 2
      Queen 'A' 2;
    move_test "generated test 24 from actual game 2" test_24_state 'E' 3 'G' 5
      Bishop 'G' 5;
    move_test "generated test 25 from actual game 2" test_25_state 'F' 7 'F' 6
      Pawn 'F' 6;
    move_test "generated test 26 from actual game 2" test_26_state 'F' 3 'G' 4
      Queen 'G' 4;
    move_test "generated test 27 from actual game 2" test_27_state 'D' 7 'D' 8
      King 'D' 8;
    move_test "generated test 28 from actual game 2" test_28_state 'B' 5 'C' 6
      Bishop 'C' 6;
    move_test "generated test 29 from actual game 2" test_29_state 'B' 7 'C' 6
      Pawn 'C' 6;
    move_test "generated test 30 from actual game 2" test_30_state 'G' 4 'E' 4
      Queen 'E' 4;
    move_test "generated test 31 from actual game 2" test_31_state 'A' 2 'A' 1
      Queen 'A' 1;
    move_test "generated test 32 from actual game 2" test_32_state 'C' 1 'D' 2
      King 'D' 2;
    move_test "generated test 33 from actual game 2" test_33_state 'D' 8 'C' 8
      King 'C' 8;
    move_test "generated test 34 from actual game 2" test_34_state 'E' 4 'C' 6
      Queen 'C' 6;
    move_test "generated test 35 from actual game 2" test_35_state 'A' 1 'D' 1
      Queen 'D' 1;
    move_test "generated test 36 from actual game 2" test_36_state 'D' 2 'D' 1
      King 'D' 1;
    move_test "generated test 37 from actual game 2" test_37_state 'F' 8 'E' 7
      Bishop 'E' 7;
    move_test "generated test 38 from actual game 2" test_38_state 'E' 1 'E' 7
      Rook 'E' 7;
    move_test "generated test 39 from actual game 2" test_39_state 'C' 8 'B' 8
      King 'B' 8;
    move_test "generated test 40 from actual game 2" test_40_state 'C' 6 'C' 7
      Queen 'C' 7;
  ]

let test_0_state = State.init_state
let test_1_state = test_0_state |> State.move 'D' 2 'D' 4 |> Option.get
let test_2_state = test_1_state |> State.move 'D' 7 'D' 5 |> Option.get
let test_3_state = test_2_state |> State.move 'C' 2 'C' 4 |> Option.get
let test_4_state = test_3_state |> State.move 'C' 7 'C' 6 |> Option.get
let test_5_state = test_4_state |> State.move 'B' 1 'C' 3 |> Option.get
let test_6_state = test_5_state |> State.move 'E' 7 'E' 6 |> Option.get
let test_7_state = test_6_state |> State.move 'E' 2 'E' 3 |> Option.get
let test_8_state = test_7_state |> State.move 'G' 8 'F' 6 |> Option.get
let test_9_state = test_8_state |> State.move 'G' 1 'F' 3 |> Option.get
let test_10_state = test_9_state |> State.move 'F' 8 'D' 6 |> Option.get
let test_11_state = test_10_state |> State.move 'C' 4 'C' 5 |> Option.get
let test_12_state = test_11_state |> State.move 'D' 6 'C' 7 |> Option.get
let test_13_state = test_12_state |> State.move 'F' 1 'D' 3 |> Option.get
let test_14_state = test_13_state |> State.move 'B' 8 'D' 7 |> Option.get
let test_15_state = test_14_state |> State.move 'E' 1 'G' 1 |> Option.get
let test_16_state = test_15_state |> State.move 'E' 6 'E' 5 |> Option.get
let test_17_state = test_16_state |> State.move 'D' 3 'F' 5 |> Option.get
let test_18_state = test_17_state |> State.move 'E' 5 'E' 4 |> Option.get
let test_19_state = test_18_state |> State.move 'F' 3 'G' 5 |> Option.get
let test_20_state = test_19_state |> State.move 'H' 7 'H' 6 |> Option.get
let test_21_state = test_20_state |> State.move 'G' 5 'H' 3 |> Option.get
let test_22_state = test_21_state |> State.move 'D' 7 'F' 8 |> Option.get
let test_23_state = test_22_state |> State.move 'F' 5 'C' 8 |> Option.get
let test_24_state = test_23_state |> State.move 'D' 8 'C' 8 |> Option.get
let test_25_state = test_24_state |> State.move 'F' 2 'F' 3 |> Option.get
let test_26_state = test_25_state |> State.move 'C' 8 'E' 6 |> Option.get
let test_27_state = test_26_state |> State.move 'F' 3 'E' 4 |> Option.get
let test_28_state = test_27_state |> State.move 'F' 6 'E' 4 |> Option.get
let test_29_state = test_28_state |> State.move 'C' 1 'D' 2 |> Option.get
let test_30_state = test_29_state |> State.move 'F' 8 'G' 6 |> Option.get
let test_31_state = test_30_state |> State.move 'C' 3 'E' 4 |> Option.get
let test_32_state = test_31_state |> State.move 'E' 6 'E' 4 |> Option.get
let test_33_state = test_32_state |> State.move 'D' 1 'H' 5 |> Option.get
let test_34_state = test_33_state |> State.move 'E' 8 'G' 8 |> Option.get
let test_35_state = test_34_state |> State.move 'G' 2 'G' 3 |> Option.get
let test_36_state = test_35_state |> State.move 'A' 8 'E' 8 |> Option.get
let test_37_state = test_36_state |> State.move 'B' 2 'B' 3 |> Option.get
let test_38_state = test_37_state |> State.move 'E' 4 'D' 3 |> Option.get
let test_39_state = test_38_state |> State.move 'F' 1 'F' 2 |> Option.get
let test_40_state = test_39_state |> State.move 'E' 8 'E' 4 |> Option.get
let test_41_state = test_40_state |> State.move 'A' 1 'F' 1 |> Option.get
let test_42_state = test_41_state |> State.move 'G' 6 'H' 8 |> Option.get
let test_43_state = test_42_state |> State.move 'F' 1 'D' 1 |> Option.get
let test_44_state = test_43_state |> State.move 'G' 7 'G' 6 |> Option.get
let test_45_state = test_44_state |> State.move 'H' 5 'H' 6 |> Option.get
let test_46_state = test_45_state |> State.move 'C' 7 'D' 8 |> Option.get
let test_47_state = test_46_state |> State.move 'D' 1 'F' 1 |> Option.get
let test_48_state = test_47_state |> State.move 'E' 4 'G' 4 |> Option.get
let test_49_state = test_48_state |> State.move 'B' 3 'B' 4 |> Option.get
let test_50_state = test_49_state |> State.move 'A' 7 'A' 6 |> Option.get
let test_51_state = test_50_state |> State.move 'A' 2 'A' 4 |> Option.get
let test_52_state = test_51_state |> State.move 'D' 8 'C' 7 |> Option.get
let test_53_state = test_52_state |> State.move 'H' 3 'G' 5 |> Option.get
let test_54_state = test_53_state |> State.move 'C' 7 'G' 3 |> Option.get

let random_game_3_tests =
  [
    move_test "generated test 0 from actual game 3" test_0_state 'D' 2 'D' 4
      Pawn 'D' 4;
    move_test "generated test 1 from actual game 3" test_1_state 'D' 7 'D' 5
      Pawn 'D' 5;
    move_test "generated test 2 from actual game 3" test_2_state 'C' 2 'C' 4
      Pawn 'C' 4;
    move_test "generated test 3 from actual game 3" test_3_state 'C' 7 'C' 6
      Pawn 'C' 6;
    move_test "generated test 4 from actual game 3" test_4_state 'B' 1 'C' 3
      Knight 'C' 3;
    move_test "generated test 5 from actual game 3" test_5_state 'E' 7 'E' 6
      Pawn 'E' 6;
    move_test "generated test 6 from actual game 3" test_6_state 'E' 2 'E' 3
      Pawn 'E' 3;
    move_test "generated test 7 from actual game 3" test_7_state 'G' 8 'F' 6
      Knight 'F' 6;
    move_test "generated test 8 from actual game 3" test_8_state 'G' 1 'F' 3
      Knight 'F' 3;
    move_test "generated test 9 from actual game 3" test_9_state 'F' 8 'D' 6
      Bishop 'D' 6;
    move_test "generated test 10 from actual game 3" test_10_state 'C' 4 'C' 5
      Pawn 'C' 5;
    move_test "generated test 11 from actual game 3" test_11_state 'D' 6 'C' 7
      Bishop 'C' 7;
    move_test "generated test 12 from actual game 3" test_12_state 'F' 1 'D' 3
      Bishop 'D' 3;
    move_test "generated test 13 from actual game 3" test_13_state 'B' 8 'D' 7
      Knight 'D' 7;
    move_test "generated test 14 from actual game 3" test_14_state 'E' 1 'G' 1
      King 'G' 1;
    move_test "generated test 15 from actual game 3" test_15_state 'E' 6 'E' 5
      Pawn 'E' 5;
    move_test "generated test 16 from actual game 3" test_16_state 'D' 3 'F' 5
      Bishop 'F' 5;
    move_test "generated test 17 from actual game 3" test_17_state 'E' 5 'E' 4
      Pawn 'E' 4;
    move_test "generated test 18 from actual game 3" test_18_state 'F' 3 'G' 5
      Knight 'G' 5;
    move_test "generated test 19 from actual game 3" test_19_state 'H' 7 'H' 6
      Pawn 'H' 6;
    move_test "generated test 20 from actual game 3" test_20_state 'G' 5 'H' 3
      Knight 'H' 3;
    move_test "generated test 21 from actual game 3" test_21_state 'D' 7 'F' 8
      Knight 'F' 8;
    move_test "generated test 22 from actual game 3" test_22_state 'F' 5 'C' 8
      Bishop 'C' 8;
    move_test "generated test 23 from actual game 3" test_23_state 'D' 8 'C' 8
      Queen 'C' 8;
    move_test "generated test 24 from actual game 3" test_24_state 'F' 2 'F' 3
      Pawn 'F' 3;
    move_test "generated test 25 from actual game 3" test_25_state 'C' 8 'E' 6
      Queen 'E' 6;
    move_test "generated test 26 from actual game 3" test_26_state 'F' 3 'E' 4
      Pawn 'E' 4;
    move_test "generated test 27 from actual game 3" test_27_state 'F' 6 'E' 4
      Knight 'E' 4;
    move_test "generated test 28 from actual game 3" test_28_state 'C' 1 'D' 2
      Bishop 'D' 2;
    move_test "generated test 29 from actual game 3" test_29_state 'F' 8 'G' 6
      Knight 'G' 6;
    move_test "generated test 30 from actual game 3" test_30_state 'C' 3 'E' 4
      Knight 'E' 4;
    move_test "generated test 31 from actual game 3" test_31_state 'E' 6 'E' 4
      Queen 'E' 4;
    move_test "generated test 32 from actual game 3" test_32_state 'D' 1 'H' 5
      Queen 'H' 5;
    move_test "generated test 33 from actual game 3" test_33_state 'E' 8 'G' 8
      King 'G' 8;
    move_test "generated test 34 from actual game 3" test_34_state 'G' 2 'G' 3
      Pawn 'G' 3;
    move_test "generated test 35 from actual game 3" test_35_state 'A' 8 'E' 8
      Rook 'E' 8;
    move_test "generated test 36 from actual game 3" test_36_state 'B' 2 'B' 3
      Pawn 'B' 3;
    move_test "generated test 37 from actual game 3" test_37_state 'E' 4 'D' 3
      Queen 'D' 3;
    move_test "generated test 38 from actual game 3" test_38_state 'F' 1 'F' 2
      Rook 'F' 2;
    move_test "generated test 39 from actual game 3" test_39_state 'E' 8 'E' 4
      Rook 'E' 4;
    move_test "generated test 40 from actual game 3" test_40_state 'A' 1 'F' 1
      Rook 'F' 1;
    move_test "generated test 41 from actual game 3" test_41_state 'G' 6 'H' 8
      Knight 'H' 8;
    move_test "generated test 42 from actual game 3" test_42_state 'F' 1 'D' 1
      Rook 'D' 1;
    move_test "generated test 43 from actual game 3" test_43_state 'G' 7 'G' 6
      Pawn 'G' 6;
    move_test "generated test 44 from actual game 3" test_44_state 'H' 5 'H' 6
      Queen 'H' 6;
    move_test "generated test 45 from actual game 3" test_45_state 'C' 7 'D' 8
      Bishop 'D' 8;
    move_test "generated test 46 from actual game 3" test_46_state 'D' 1 'F' 1
      Rook 'F' 1;
    move_test "generated test 47 from actual game 3" test_47_state 'E' 4 'G' 4
      Rook 'G' 4;
    move_test "generated test 48 from actual game 3" test_48_state 'B' 3 'B' 4
      Pawn 'B' 4;
    move_test "generated test 49 from actual game 3" test_49_state 'A' 7 'A' 6
      Pawn 'A' 6;
    move_test "generated test 50 from actual game 3" test_50_state 'A' 2 'A' 4
      Pawn 'A' 4;
    move_test "generated test 51 from actual game 3" test_51_state 'D' 8 'C' 7
      Bishop 'C' 7;
    move_test "generated test 52 from actual game 3" test_52_state 'H' 3 'G' 5
      Knight 'G' 5;
    move_test "generated test 53 from actual game 3" test_53_state 'C' 7 'G' 3
      Bishop 'G' 3;
    move_test "generated test 54 from actual game 3" test_54_state 'H' 6 'H' 7
      Queen 'H' 7;
  ]

let test_0_state = State.init_state
let test_1_state = test_0_state |> State.move 'D' 2 'D' 4 |> Option.get
let test_2_state = test_1_state |> State.move 'D' 7 'D' 5 |> Option.get
let test_3_state = test_2_state |> State.move 'C' 2 'C' 4 |> Option.get
let test_4_state = test_3_state |> State.move 'E' 7 'E' 6 |> Option.get
let test_5_state = test_4_state |> State.move 'B' 1 'C' 3 |> Option.get
let test_6_state = test_5_state |> State.move 'C' 7 'C' 6 |> Option.get
let test_7_state = test_6_state |> State.move 'E' 2 'E' 3 |> Option.get
let test_8_state = test_7_state |> State.move 'G' 8 'F' 6 |> Option.get
let test_9_state = test_8_state |> State.move 'G' 1 'F' 3 |> Option.get
let test_10_state = test_9_state |> State.move 'F' 8 'D' 6 |> Option.get
let test_11_state = test_10_state |> State.move 'F' 1 'E' 2 |> Option.get
let test_12_state = test_11_state |> State.move 'B' 8 'D' 7 |> Option.get
let test_13_state = test_12_state |> State.move 'C' 4 'C' 5 |> Option.get
let test_14_state = test_13_state |> State.move 'D' 6 'C' 7 |> Option.get
let test_15_state = test_14_state |> State.move 'E' 1 'G' 1 |> Option.get
let test_16_state = test_15_state |> State.move 'E' 6 'E' 5 |> Option.get
let test_17_state = test_16_state |> State.move 'D' 4 'E' 5 |> Option.get
let test_18_state = test_17_state |> State.move 'D' 7 'E' 5 |> Option.get
let test_19_state = test_18_state |> State.move 'D' 1 'D' 4 |> Option.get
let test_20_state = test_19_state |> State.move 'E' 5 'F' 3 |> Option.get
let test_21_state = test_20_state |> State.move 'E' 2 'F' 3 |> Option.get
let test_22_state = test_21_state |> State.move 'C' 8 'F' 5 |> Option.get
let test_23_state = test_22_state |> State.move 'G' 2 'G' 4 |> Option.get
let test_24_state = test_23_state |> State.move 'F' 5 'G' 6 |> Option.get
let test_25_state = test_24_state |> State.move 'D' 4 'B' 4 |> Option.get
let test_26_state = test_25_state |> State.move 'D' 8 'C' 8 |> Option.get
let test_27_state = test_26_state |> State.move 'G' 4 'G' 5 |> Option.get
let test_28_state = test_27_state |> State.move 'F' 6 'D' 7 |> Option.get
let test_29_state = test_28_state |> State.move 'E' 3 'E' 4 |> Option.get
let test_30_state = test_29_state |> State.move 'D' 5 'E' 4 |> Option.get
let test_31_state = test_30_state |> State.move 'F' 3 'E' 4 |> Option.get
let test_32_state = test_31_state |> State.move 'E' 8 'G' 8 |> Option.get
let test_33_state = test_32_state |> State.move 'F' 1 'D' 1 |> Option.get
let test_34_state = test_33_state |> State.move 'D' 7 'E' 5 |> Option.get
let test_35_state = test_34_state |> State.move 'F' 2 'F' 4 |> Option.get
let test_36_state = test_35_state |> State.move 'C' 8 'G' 4 |> Option.get
let test_37_state = test_36_state |> State.move 'G' 1 'F' 1 |> Option.get
let test_38_state = test_37_state |> State.move 'E' 5 'F' 3 |> Option.get
let test_39_state = test_38_state |> State.move 'E' 4 'F' 3 |> Option.get
let test_40_state = test_39_state |> State.move 'G' 4 'F' 3 |> Option.get
let test_41_state = test_40_state |> State.move 'F' 1 'G' 1 |> Option.get
let test_42_state = test_41_state |> State.move 'A' 8 'E' 8 |> Option.get
let test_43_state = test_42_state |> State.move 'C' 1 'D' 2 |> Option.get
let test_44_state = test_43_state |> State.move 'G' 6 'E' 4 |> Option.get
let test_45_state = test_44_state |> State.move 'C' 3 'E' 4 |> Option.get
let test_46_state = test_45_state |> State.move 'E' 8 'E' 4 |> Option.get
let test_47_state = test_46_state |> State.move 'B' 4 'B' 3 |> Option.get
let test_48_state = test_47_state |> State.move 'F' 3 'H' 5 |> Option.get
let test_49_state = test_48_state |> State.move 'B' 3 'B' 7 |> Option.get
let test_50_state = test_49_state |> State.move 'C' 7 'F' 4 |> Option.get
let test_51_state = test_50_state |> State.move 'D' 2 'F' 4 |> Option.get
let test_52_state = test_51_state |> State.move 'E' 4 'F' 4 |> Option.get
let test_53_state = test_52_state |> State.move 'B' 7 'C' 6 |> Option.get
let test_54_state = test_53_state |> State.move 'H' 5 'G' 5 |> Option.get
let test_55_state = test_54_state |> State.move 'G' 1 'H' 1 |> Option.get
let test_56_state = test_55_state |> State.move 'G' 5 'F' 5 |> Option.get
let test_57_state = test_56_state |> State.move 'C' 6 'B' 5 |> Option.get
let test_58_state = test_57_state |> State.move 'F' 5 'E' 4 |> Option.get
let test_59_state = test_58_state |> State.move 'H' 1 'G' 1 |> Option.get
let test_60_state = test_59_state |> State.move 'F' 4 'G' 4 |> Option.get
let test_61_state = test_60_state |> State.move 'G' 1 'F' 2 |> Option.get
let test_62_state = test_61_state |> State.move 'G' 4 'G' 2 |> Option.get
let test_63_state = test_62_state |> State.move 'F' 2 'F' 1 |> Option.get
let test_64_state = test_63_state |> State.move 'E' 4 'F' 3 |> Option.get
let test_65_state = test_64_state |> State.move 'F' 1 'E' 1 |> Option.get

let random_game_4_tests =
  [
    move_test "generated test 0 from actual game 4" test_0_state 'D' 2 'D' 4
      Pawn 'D' 4;
    move_test "generated test 1 from actual game 4" test_1_state 'D' 7 'D' 5
      Pawn 'D' 5;
    move_test "generated test 2 from actual game 4" test_2_state 'C' 2 'C' 4
      Pawn 'C' 4;
    move_test "generated test 3 from actual game 4" test_3_state 'E' 7 'E' 6
      Pawn 'E' 6;
    move_test "generated test 4 from actual game 4" test_4_state 'B' 1 'C' 3
      Knight 'C' 3;
    move_test "generated test 5 from actual game 4" test_5_state 'C' 7 'C' 6
      Pawn 'C' 6;
    move_test "generated test 6 from actual game 4" test_6_state 'E' 2 'E' 3
      Pawn 'E' 3;
    move_test "generated test 7 from actual game 4" test_7_state 'G' 8 'F' 6
      Knight 'F' 6;
    move_test "generated test 8 from actual game 4" test_8_state 'G' 1 'F' 3
      Knight 'F' 3;
    move_test "generated test 9 from actual game 4" test_9_state 'F' 8 'D' 6
      Bishop 'D' 6;
    move_test "generated test 10 from actual game 4" test_10_state 'F' 1 'E' 2
      Bishop 'E' 2;
    move_test "generated test 11 from actual game 4" test_11_state 'B' 8 'D' 7
      Knight 'D' 7;
    move_test "generated test 12 from actual game 4" test_12_state 'C' 4 'C' 5
      Pawn 'C' 5;
    move_test "generated test 13 from actual game 4" test_13_state 'D' 6 'C' 7
      Bishop 'C' 7;
    move_test "generated test 14 from actual game 4" test_14_state 'E' 1 'G' 1
      King 'G' 1;
    move_test "generated test 15 from actual game 4" test_15_state 'E' 6 'E' 5
      Pawn 'E' 5;
    move_test "generated test 16 from actual game 4" test_16_state 'D' 4 'E' 5
      Pawn 'E' 5;
    move_test "generated test 17 from actual game 4" test_17_state 'D' 7 'E' 5
      Knight 'E' 5;
    move_test "generated test 18 from actual game 4" test_18_state 'D' 1 'D' 4
      Queen 'D' 4;
    move_test "generated test 19 from actual game 4" test_19_state 'E' 5 'F' 3
      Knight 'F' 3;
    move_test "generated test 20 from actual game 4" test_20_state 'E' 2 'F' 3
      Bishop 'F' 3;
    move_test "generated test 21 from actual game 4" test_21_state 'C' 8 'F' 5
      Bishop 'F' 5;
    move_test "generated test 22 from actual game 4" test_22_state 'G' 2 'G' 4
      Pawn 'G' 4;
    move_test "generated test 23 from actual game 4" test_23_state 'F' 5 'G' 6
      Bishop 'G' 6;
    move_test "generated test 24 from actual game 4" test_24_state 'D' 4 'B' 4
      Queen 'B' 4;
    move_test "generated test 25 from actual game 4" test_25_state 'D' 8 'C' 8
      Queen 'C' 8;
    move_test "generated test 26 from actual game 4" test_26_state 'G' 4 'G' 5
      Pawn 'G' 5;
    move_test "generated test 27 from actual game 4" test_27_state 'F' 6 'D' 7
      Knight 'D' 7;
    move_test "generated test 28 from actual game 4" test_28_state 'E' 3 'E' 4
      Pawn 'E' 4;
    move_test "generated test 29 from actual game 4" test_29_state 'D' 5 'E' 4
      Pawn 'E' 4;
    move_test "generated test 30 from actual game 4" test_30_state 'F' 3 'E' 4
      Bishop 'E' 4;
    move_test "generated test 31 from actual game 4" test_31_state 'E' 8 'G' 8
      King 'G' 8;
    move_test "generated test 32 from actual game 4" test_32_state 'F' 1 'D' 1
      Rook 'D' 1;
    move_test "generated test 33 from actual game 4" test_33_state 'D' 7 'E' 5
      Knight 'E' 5;
    move_test "generated test 34 from actual game 4" test_34_state 'F' 2 'F' 4
      Pawn 'F' 4;
    move_test "generated test 35 from actual game 4" test_35_state 'C' 8 'G' 4
      Queen 'G' 4;
    move_test "generated test 36 from actual game 4" test_36_state 'G' 1 'F' 1
      King 'F' 1;
    move_test "generated test 37 from actual game 4" test_37_state 'E' 5 'F' 3
      Knight 'F' 3;
    move_test "generated test 38 from actual game 4" test_38_state 'E' 4 'F' 3
      Bishop 'F' 3;
    move_test "generated test 39 from actual game 4" test_39_state 'G' 4 'F' 3
      Queen 'F' 3;
    move_test "generated test 40 from actual game 4" test_40_state 'F' 1 'G' 1
      King 'G' 1;
    move_test "generated test 41 from actual game 4" test_41_state 'A' 8 'E' 8
      Rook 'E' 8;
    move_test "generated test 42 from actual game 4" test_42_state 'C' 1 'D' 2
      Bishop 'D' 2;
    move_test "generated test 43 from actual game 4" test_43_state 'G' 6 'E' 4
      Bishop 'E' 4;
    move_test "generated test 44 from actual game 4" test_44_state 'C' 3 'E' 4
      Knight 'E' 4;
    move_test "generated test 45 from actual game 4" test_45_state 'E' 8 'E' 4
      Rook 'E' 4;
    move_test "generated test 46 from actual game 4" test_46_state 'B' 4 'B' 3
      Queen 'B' 3;
    move_test "generated test 47 from actual game 4" test_47_state 'F' 3 'H' 5
      Queen 'H' 5;
    move_test "generated test 48 from actual game 4" test_48_state 'B' 3 'B' 7
      Queen 'B' 7;
    move_test "generated test 49 from actual game 4" test_49_state 'C' 7 'F' 4
      Bishop 'F' 4;
    move_test "generated test 50 from actual game 4" test_50_state 'D' 2 'F' 4
      Bishop 'F' 4;
    move_test "generated test 51 from actual game 4" test_51_state 'E' 4 'F' 4
      Rook 'F' 4;
    move_test "generated test 52 from actual game 4" test_52_state 'B' 7 'C' 6
      Queen 'C' 6;
    move_test "generated test 53 from actual game 4" test_53_state 'H' 5 'G' 5
      Queen 'G' 5;
    move_test "generated test 54 from actual game 4" test_54_state 'G' 1 'H' 1
      King 'H' 1;
    move_test "generated test 55 from actual game 4" test_55_state 'G' 5 'F' 5
      Queen 'F' 5;
    move_test "generated test 56 from actual game 4" test_56_state 'C' 6 'B' 5
      Queen 'B' 5;
    move_test "generated test 57 from actual game 4" test_57_state 'F' 5 'E' 4
      Queen 'E' 4;
    move_test "generated test 58 from actual game 4" test_58_state 'H' 1 'G' 1
      King 'G' 1;
    move_test "generated test 59 from actual game 4" test_59_state 'F' 4 'G' 4
      Rook 'G' 4;
    move_test "generated test 60 from actual game 4" test_60_state 'G' 1 'F' 2
      King 'F' 2;
    move_test "generated test 61 from actual game 4" test_61_state 'G' 4 'G' 2
      Rook 'G' 2;
    move_test "generated test 62 from actual game 4" test_62_state 'F' 2 'F' 1
      King 'F' 1;
    move_test "generated test 63 from actual game 4" test_63_state 'E' 4 'F' 3
      Queen 'F' 3;
    move_test "generated test 64 from actual game 4" test_64_state 'F' 1 'E' 1
      King 'E' 1;
    move_test "generated test 65 from actual game 4" test_65_state 'F' 3 'F' 2
      Queen 'F' 2;
  ]

let test_0_state = State.init_state
let test_1_state = test_0_state |> State.move 'D' 2 'D' 4 |> Option.get
let test_2_state = test_1_state |> State.move 'G' 8 'F' 6 |> Option.get
let test_3_state = test_2_state |> State.move 'C' 2 'C' 4 |> Option.get
let test_4_state = test_3_state |> State.move 'E' 7 'E' 6 |> Option.get
let test_5_state = test_4_state |> State.move 'A' 2 'A' 3 |> Option.get
let test_6_state = test_5_state |> State.move 'F' 8 'E' 7 |> Option.get
let test_7_state = test_6_state |> State.move 'B' 1 'C' 3 |> Option.get
let test_8_state = test_7_state |> State.move 'E' 8 'G' 8 |> Option.get
let test_9_state = test_8_state |> State.move 'E' 2 'E' 4 |> Option.get
let test_10_state = test_9_state |> State.move 'D' 7 'D' 6 |> Option.get
let test_11_state = test_10_state |> State.move 'C' 1 'E' 3 |> Option.get
let test_12_state = test_11_state |> State.move 'B' 8 'C' 6 |> Option.get
let test_13_state = test_12_state |> State.move 'F' 1 'E' 2 |> Option.get
let test_14_state = test_13_state |> State.move 'D' 6 'D' 5 |> Option.get
let test_15_state = test_14_state |> State.move 'E' 4 'E' 5 |> Option.get
let test_16_state = test_15_state |> State.move 'F' 6 'E' 4 |> Option.get
let test_17_state = test_16_state |> State.move 'C' 4 'C' 5 |> Option.get
let test_18_state = test_17_state |> State.move 'E' 4 'C' 3 |> Option.get
let test_19_state = test_18_state |> State.move 'B' 2 'C' 3 |> Option.get
let test_20_state = test_19_state |> State.move 'B' 7 'B' 6 |> Option.get
let test_21_state = test_20_state |> State.move 'G' 1 'F' 3 |> Option.get
let test_22_state = test_21_state |> State.move 'B' 6 'C' 5 |> Option.get
let test_23_state = test_22_state |> State.move 'D' 4 'C' 5 |> Option.get
let test_24_state = test_23_state |> State.move 'F' 7 'F' 6 |> Option.get
let test_25_state = test_24_state |> State.move 'E' 5 'F' 6 |> Option.get
let test_26_state = test_25_state |> State.move 'E' 7 'F' 6 |> Option.get
let test_27_state = test_26_state |> State.move 'A' 1 'C' 1 |> Option.get
let test_28_state = test_27_state |> State.move 'A' 8 'B' 8 |> Option.get
let test_29_state = test_28_state |> State.move 'E' 1 'G' 1 |> Option.get
let test_30_state = test_29_state |> State.move 'G' 8 'H' 8 |> Option.get
let test_31_state = test_30_state |> State.move 'E' 3 'D' 4 |> Option.get
let test_32_state = test_31_state |> State.move 'E' 6 'E' 5 |> Option.get
let test_33_state = test_32_state |> State.move 'D' 4 'E' 5 |> Option.get
let test_34_state = test_33_state |> State.move 'F' 6 'E' 5 |> Option.get
let test_35_state = test_34_state |> State.move 'F' 3 'E' 5 |> Option.get
let test_36_state = test_35_state |> State.move 'C' 6 'E' 5 |> Option.get
let test_37_state = test_36_state |> State.move 'D' 1 'D' 4 |> Option.get
let test_38_state = test_37_state |> State.move 'E' 5 'C' 6 |> Option.get
let test_39_state = test_38_state |> State.move 'D' 4 'G' 4 |> Option.get

let random_game_5_tests =
  [
    move_test "generated test 0 from actual game 5" test_0_state 'D' 2 'D' 4
      Pawn 'D' 4;
    move_test "generated test 1 from actual game 5" test_1_state 'G' 8 'F' 6
      Knight 'F' 6;
    move_test "generated test 2 from actual game 5" test_2_state 'C' 2 'C' 4
      Pawn 'C' 4;
    move_test "generated test 3 from actual game 5" test_3_state 'E' 7 'E' 6
      Pawn 'E' 6;
    move_test "generated test 4 from actual game 5" test_4_state 'A' 2 'A' 3
      Pawn 'A' 3;
    move_test "generated test 5 from actual game 5" test_5_state 'F' 8 'E' 7
      Bishop 'E' 7;
    move_test "generated test 6 from actual game 5" test_6_state 'B' 1 'C' 3
      Knight 'C' 3;
    move_test "generated test 7 from actual game 5" test_7_state 'E' 8 'G' 8
      King 'G' 8;
    move_test "generated test 8 from actual game 5" test_8_state 'E' 2 'E' 4
      Pawn 'E' 4;
    move_test "generated test 9 from actual game 5" test_9_state 'D' 7 'D' 6
      Pawn 'D' 6;
    move_test "generated test 10 from actual game 5" test_10_state 'C' 1 'E' 3
      Bishop 'E' 3;
    move_test "generated test 11 from actual game 5" test_11_state 'B' 8 'C' 6
      Knight 'C' 6;
    move_test "generated test 12 from actual game 5" test_12_state 'F' 1 'E' 2
      Bishop 'E' 2;
    move_test "generated test 13 from actual game 5" test_13_state 'D' 6 'D' 5
      Pawn 'D' 5;
    move_test "generated test 14 from actual game 5" test_14_state 'E' 4 'E' 5
      Pawn 'E' 5;
    move_test "generated test 15 from actual game 5" test_15_state 'F' 6 'E' 4
      Knight 'E' 4;
    move_test "generated test 16 from actual game 5" test_16_state 'C' 4 'C' 5
      Pawn 'C' 5;
    move_test "generated test 17 from actual game 5" test_17_state 'E' 4 'C' 3
      Knight 'C' 3;
    move_test "generated test 18 from actual game 5" test_18_state 'B' 2 'C' 3
      Pawn 'C' 3;
    move_test "generated test 19 from actual game 5" test_19_state 'B' 7 'B' 6
      Pawn 'B' 6;
    move_test "generated test 20 from actual game 5" test_20_state 'G' 1 'F' 3
      Knight 'F' 3;
    move_test "generated test 21 from actual game 5" test_21_state 'B' 6 'C' 5
      Pawn 'C' 5;
    move_test "generated test 22 from actual game 5" test_22_state 'D' 4 'C' 5
      Pawn 'C' 5;
    move_test "generated test 23 from actual game 5" test_23_state 'F' 7 'F' 6
      Pawn 'F' 6;
    move_test "generated test 24 from actual game 5" test_24_state 'E' 5 'F' 6
      Pawn 'F' 6;
    move_test "generated test 25 from actual game 5" test_25_state 'E' 7 'F' 6
      Bishop 'F' 6;
    move_test "generated test 26 from actual game 5" test_26_state 'A' 1 'C' 1
      Rook 'C' 1;
    move_test "generated test 27 from actual game 5" test_27_state 'A' 8 'B' 8
      Rook 'B' 8;
    move_test "generated test 28 from actual game 5" test_28_state 'E' 1 'G' 1
      King 'G' 1;
    move_test "generated test 29 from actual game 5" test_29_state 'G' 8 'H' 8
      King 'H' 8;
    move_test "generated test 30 from actual game 5" test_30_state 'E' 3 'D' 4
      Bishop 'D' 4;
    move_test "generated test 31 from actual game 5" test_31_state 'E' 6 'E' 5
      Pawn 'E' 5;
    move_test "generated test 32 from actual game 5" test_32_state 'D' 4 'E' 5
      Bishop 'E' 5;
    move_test "generated test 33 from actual game 5" test_33_state 'F' 6 'E' 5
      Bishop 'E' 5;
    move_test "generated test 34 from actual game 5" test_34_state 'F' 3 'E' 5
      Knight 'E' 5;
    move_test "generated test 35 from actual game 5" test_35_state 'C' 6 'E' 5
      Knight 'E' 5;
    move_test "generated test 36 from actual game 5" test_36_state 'D' 1 'D' 4
      Queen 'D' 4;
    move_test "generated test 37 from actual game 5" test_37_state 'E' 5 'C' 6
      Knight 'C' 6;
    move_test "generated test 38 from actual game 5" test_38_state 'D' 4 'G' 4
      Queen 'G' 4;
    move_test "generated test 39 from actual game 5" test_39_state 'C' 8 'G' 4
      Bishop 'G' 4;
  ]

let suite =
  "test suite for chessmon"
  >::: List.flatten
         [
           state_tests;
           board_tests;
           random_game_1_tests;
           random_game_2_tests;
           random_game_3_tests;
           random_game_4_tests;
           random_game_5_tests;
         ]

let _ = run_test_tt_main suite
