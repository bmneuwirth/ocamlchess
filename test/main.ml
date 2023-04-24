open OUnit2
open Chessmon
open State
open Board

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

let check_if_occupied_test (name : string) (board : Board.board) (c : char)
    (i : int) (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (Board.check_if_occupied board c (i + 4))

let board_tests =
  [ check_if_occupied_test "check starting piece" Board.init_board 'A' 1 true ]

let suite =
  "test suite for chessmon" >::: List.flatten [ state_tests; board_tests ]

let _ = run_test_tt_main suite
