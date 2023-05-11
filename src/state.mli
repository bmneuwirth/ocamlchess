open Board

type player_state = { can_castle_left : bool; can_castle_right : bool }

type state = {
  board : board;
  color : color;
  white_state : player_state;
  black_state : player_state;
  can_promote : bool;
}

val init_state : state
(** [init_state] is the initial state of the game. *)

val move : char -> int -> char -> int -> state -> state option
(** [move start_col start_row end_col end_row state] is Some new_state if the 
    move is valid, where new_state is the state after the move. If the move is
invalid, the result is None. *)

val print_command : state -> unit
(** [print_command state] prints the command for inputting the next move to the
    state. *)

(*
   val update_state : Board.board -> Board.piece -> char -> int -> board option
   (** [update_state board piece column row board] is the result of moving a
       [piece] to a position given with [column] and [row]:

       - Precondition: The move was already check to be valid.

       Effects: returns a board with the given piece moved. *) *)