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

val remove_piece : board -> char -> int -> board
(** [remove_piece b col row] returns the board [b] with the piece at row [r] and 
col [c] removed. *)

val print_board : board -> unit
(** [print_board board] prints out a representation of the current board [board]. *)

val promote : board -> piece_type -> board
(** [promote] returns a board that contains the updated piece with type [promote_to_piece_type]. *)

val check_pawn_end_pos : piece -> char -> int -> bool
(** [check_pawn_end_pos piece c i] is a bool that checks if moving [piece] of 
  piece_type Pawn to row [i] and column [c] is legal or not. Returns true if 
    legal, false if not. *)

val check_knight_end_pos : piece -> char -> int -> bool
(** [check_knight_end_pos piece c i] is a bool that checks if moving [piece] of 
  piece_type Knight to row [i] and column [c] is legal or not. Returns true if 
    legal, false if not. *)

val check_bishop_end_pos : piece -> char -> int -> bool
(** [check_bishop_end_pos piece c i] is a bool that checks if moving [piece] of 
  piece_type Bishop to row [i] and column [c] is legal or not. Returns true if 
    legal, false if not. *)

val check_rook_end_pos : piece -> char -> int -> bool
(** [check_rook_end_pos piece c i] is a bool that checks if moving [piece] of 
  piece_type Rook to row [i] and column [c] is legal or not. Returns true if 
    legal, false if not. *)

val check_queen_end_pos : piece -> char -> int -> bool
(** [check_queen_move piece c i] is a bool that checks if moving [piece] of 
  piece_type Queen to row [i] and column [c] is legal or not. Returns true if 
    legal, false if not. *)

val check_king_end_pos : piece -> char -> int -> bool
(** [check_king_move piece c i] is a bool that checks if moving [piece] of 
  piece_type King to row [i] and column [c] is legal or not. Returns true if 
    legal, false if not. *)

val check_if_occupied : board -> char -> int -> bool
(** [check_if_occupied board c i ] is a boolean that returns whether the square 
represented by column [c] and row [i] is currently occupied (another piece is on
the square represented by column [c] and row [i]). Returns true if occupied, 
  false if not *)

val next_square : piece -> char * int -> char * int -> char * int
(** [next_square piece (start_col, start_row) (end_col, end_row)] returns the 
next square on the path of [piece] from [(start_col, start_row)] to 
[(end_col, end_row)] *)

val find_path : board -> piece -> char * int -> char * int -> (char * int) list
(** [find_path board piece (start_col, start_row) (end_col, end_row)] returns a 
list of squares that [piece] takes in its path from [(start_col, start_row)] to 
[(end_col, end_row)] *)

val check_each_square : board -> (char * int) list -> bool
(** [check_each_square board lst] checks if each square in [lst] is occupied. 
  If none are occupied, returns true. If at least one square is occupied, 
returns false.*)

val check_btwn_squares : board -> piece -> char -> int -> bool
(** [check_pawn_btwn_squares board piece c i] checks if all the squares in the 
  path (piece.column, piece.row) to [(c, i)] are unoccupied. Returns true if so, 
  false if not. Requires: [(c, i)] is a valid end_pos for [piece] *)

val check_valid_move : board -> piece -> char -> int -> bool
(** [check_valid_move board piece c i] finds what type the [piece] is and calls 
function to check if moving [piece] to column [c] and row [i] is valid based on
its type. Returns true if legal move, false if not. *)

val check_valid_piece_on_board : board -> board -> piece -> char -> int -> bool
(** [check_valid_piece_on_board piece c i] is a boolean that returns 
whether moving [piece] to column [c] and row [i] is a legal move or not. 
Returns true if the move is legal, and returns false if the move is not legal. *)

val is_capture : board -> piece -> char -> int -> bool
(** [is_captured board piece col row] is a boolean that returns true if 
  moving [piece] to column [col] and row [row] would result in a capture, 
returns false if not. *)

val is_check : board -> color -> bool
(** [is_check board color] returns boolean on whether the [color] king is in 
check or not on the [board] *)

val is_mate : board -> color -> bool
(** [is_mate board color] returns boolean on whether the [color] king is mated or not on the [board] *)

val move : board -> char -> int -> char -> int -> bool -> bool -> board option
(** [move board c1 i1 c2 i2 b1 b2] returns a board after applying the given move.
    The move is represented by [c1 i1 c2 i2], where c1i1 is the location of the
    piece to move, c2 is the letter representing the column to move to, and i2 is 
    the int representing the row to move to. [b1] represents whether the player is allowed
    to castle left. [b2] represents whether the player is allowed to castle right.
    
    If the move is invalid, [None] is returned, and if the move is valid, 
    [Some new_board] is returned where [new_board] is the updated board. *)
