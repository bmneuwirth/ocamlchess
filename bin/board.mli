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

val print_board : board -> unit
(** [print_board board] prints out a representation of the current board [board]. *)

val move : board -> piece_type -> char -> int -> board option
(** [move board piece_type c i] returns a board after applying the given move.
    The move is represented by [piece_type c i], where piece_type is the piece type
    to be moved, c is the letter representing the column to move to, and i is 
    the int representing the row to move to. 
    
    If the move is invalid, [None] is returned, and if the move is valid, 
    [Some new_board] is returned where [new_board] is the updated board. *)
