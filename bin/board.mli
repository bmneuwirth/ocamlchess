type color =
  | Black
  | White  (** The type [color] represents the color of a particular piece. *)

type piece_type =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King
      (** The type [piece_type] represents the type of a particular piece. *)

type board
(** The type [board] represents the state of the chess board. *)

val init_board : board
(** [init_board] returns the initial chess board at the start of a game. *)

val get_piece_color : board -> char -> int -> color option
(** [get_piece_color board col row] gets the color of the piece at position (col, row), or none if there is no piece there. *)

val print_board : board -> unit
(** [print_board board] prints out a representation of the current board [board]. *)

val move : board -> char -> int -> char -> int -> board option
(** [move board c1 i1 c2 i2] returns a board after applying the given move.
    The move is represented by [c1 i1 c2 i2], where c1i1 is the location of the
    piece to move, c2 is the letter representing the column to move to, and i2 is 
    the int representing the row to move to. 
    
    If the move is invalid, [None] is returned, and if the move is valid, 
    [Some new_board] is returned where [new_board] is the updated board. *)
