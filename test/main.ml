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
  {
    piece_type = Pawn;
    color = Black;
    column = 'F';
    row = 5;
    en_passant_eligble = false;
  }

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
      'D' 8
    (*
    ( "white can't castle right after king move" >:: fun _ ->
      assert_equal false neither_can_castle.white_state.can_castle_right );
    ( "white can't castle left after king move" >:: fun _ ->
      assert_equal false neither_can_castle.white_state.can_castle_left );
    ( "black can't castle right after king move" >:: fun _ ->
      assert_equal false neither_can_castle.black_state.can_castle_right );
    ( "black can't castle left after king move" >:: fun _ ->
      assert_equal false neither_can_castle.black_state.can_castle_left );
    *);
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

let find_piece_type_test (name : string) board (piece : Board.piece) (c : char)
    (i : int) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (Board.find_piece_type board piece c i)

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
    find_piece_type_test "returns false if square is occupied - pawn"
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
    find_piece_type_test "returns true if no square occupied - bishop"
      Board.(move init_board 'G' 2 'G' 4 false false |> extract_board)
      { piece_type = Bishop; color = White; column = 'F'; row = 1 }
      'H' 3 true;
    find_piece_type_test "returns false if square occupied - bishop"
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
    find_piece_type_test "returns true if no square occupied - rook"
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
    find_piece_type_test "returns false if square occupied - rook"
      Board.(move init_board 'H' 2 'H' 4 false false |> extract_board)
      { piece_type = Rook; color = White; column = 'H'; row = 1 }
      'H' 6 false;
    find_piece_type_test "testing queen rows"
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
    find_piece_type_test "testing queen columns"
      Board.(move init_board 'D' 2 'D' 4 false false |> extract_board)
      { piece_type = Queen; color = White; column = 'D'; row = 1 }
      'D' 6 false;
    find_piece_type_test "testing queen diagonals" Board.init_board
      { piece_type = Queen; color = Black; column = 'D'; row = 8 }
      'H' 4 false;
    find_piece_type_test "testing queen diagonals - true"
      Board.(move init_board 'E' 7 'E' 5 false false |> extract_board)
      { piece_type = Queen; color = Black; column = 'D'; row = 8 }
      'H' 4 true;
    ( "is the black pawn there" >:: fun _ ->
      assert_equal (Some test_pawn)
        (Board.get_piece weird_board_test.board 'F' 5) );
  ]

let suite =
  "test suite for chessmon" >::: List.flatten [ state_tests; board_tests ]

let _ = run_test_tt_main suite
