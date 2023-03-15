open Board

(** The abstract type of values representing the game state.*)

val update_state :
  Board.board -> Board.piece_type -> char -> int -> board option
(** [update_state board piece column row board] is the result of moving a 
    [peice_type] to a position given with [column] and [row]:

    - Precondition: The move was already check to be valid.

    Effects: returns a board with the given piece moved. *)
