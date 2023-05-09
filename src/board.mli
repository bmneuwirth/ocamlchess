exception King_not_found
exception CheckMate

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

type piece = {
  piece_type : piece_type;
  color : color;
  column : char;
  row : int;
  en_passant_eligble : bool;
}
(** The type [piece] represents a particular piece on the board. *)

type board
(** The type [board] represents the state of the chess board. *)

val init_board : board
(** [init_board] returns the initial chess board at the start of a game. *)

val get_piece : board -> char -> int -> piece option
(** [get_piece b col row] returns the piece on board [b] at row [r] and col [c],
   where row is a int and col is a char. *)

val get_piece_color : board -> char -> int -> color option
(** [get_piece_color board col row] returns [Some color] if there is a piece at 
    position (col, row) with color [color], or [None] if there is no piece there. *)

val print_board : board -> unit
(** [print_board board] prints out a representation of the current board [board]. *)

val promote : board -> piece_type -> board
(** [promote] returns a board that contains the updated piece with type [promote_to_piece_type]. *)

val move : board -> char -> int -> char -> int -> bool -> bool -> board option
(** [move board c1 i1 c2 i2 b1 b2] returns a board after applying the given move.
    The move is represented by [c1 i1 c2 i2], where c1i1 is the location of the
    piece to move, c2 is the letter representing the column to move to, and i2 is 
    the int representing the row to move to. [b1] represents whether the player is allowed
    to castle left. [b2] represents whether the player is allowed to castle right.
    
    If the move is invalid, [None] is returned, and if the move is valid, 
    [Some new_board] is returned where [new_board] is the updated board. *)
