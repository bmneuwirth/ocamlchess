open Board

val update_state : Board.board -> Board.piece -> char -> int -> board option
(** [update_state board piece column row board] is the result of moving a 
    [piece] to a position given with [column] and [row]:

    - Precondition: The move was already check to be valid.

    Effects: returns a board with the given piece moved. *)
