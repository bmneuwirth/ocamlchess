type color = Black | White
type piece_type = Pawn | Knight | Bishop | Rook | Queen | King

type piece = {
  piece_type : piece_type;
  color : color;
  column : char;
  row : int;
}

type board = piece list

let board_width = 8
let board_height = 8
let make_piece piece_type color column row = { piece_type; color; column; row }

let init_pieces color : board =
  let pawn_start = if color = White then 7 else 2 in
  let back_start = if color = White then 8 else 1 in
  [
    make_piece Pawn color 'A' pawn_start;
    make_piece Pawn color 'B' pawn_start;
    make_piece Pawn color 'C' pawn_start;
    make_piece Pawn color 'D' pawn_start;
    make_piece Pawn color 'E' pawn_start;
    make_piece Pawn color 'F' pawn_start;
    make_piece Pawn color 'G' pawn_start;
    make_piece Pawn color 'H' pawn_start;
    make_piece Rook color 'A' back_start;
    make_piece Rook color 'H' back_start;
    make_piece Knight color 'B' back_start;
    make_piece Knight color 'G' back_start;
    make_piece Bishop color 'C' back_start;
    make_piece Bishop color 'F' back_start;
    make_piece Queen color 'D' back_start;
    make_piece King color 'E' back_start;
  ]

let init_board = init_pieces White @ init_pieces Black
let col_char_to_int col = Char.chr (col + Char.code 'A' - 1)

let piece_type_to_char p =
  match p with
  | Pawn -> 'P'
  | Rook -> 'R'
  | Knight -> 'N'
  | Bishop -> 'B'
  | Queen -> 'Q'
  | King -> 'K'

let get_piece_char b row col =
  match
    List.filter (fun x -> x.column = col_char_to_int col && x.row = row) b
  with
  | [] -> '.'
  | piece :: _ -> piece_type_to_char piece.piece_type

let print_board b =
  for row = 1 to board_height do
    for col = 1 to board_width do
      print_char (get_piece_char b row col);
      if col <> board_width then print_char ' ' else ()
    done;
    print_endline ""
  done

(** [check_pawn_move piece c i] is a bool that checks if moving [piece] of 
  piece_type Pawn to row [r] and column [c] is legal or not. Returns true if 
    legal, false if not. *)
let check_pawn_move piece c i =
  if
    (c = piece.column && (i = piece.row + 1 || i = piece.row + 2))
    || i = piece.row
       && (c = Char.chr (Char.code piece.column + 1)
          || c = Char.chr (Char.code piece.column - 1))
  then true
  else false

(** [check_knight_move piece c i] is a bool that checks if moving [piece] of 
  piece_type Knight to row [r] and column [c] is legal or not. Returns true if 
    legal, false if not. *)
let check_knight_move piece c i =
  if
    (i = piece.row + 1 || i = piece.row - 1)
    && (c = Char.chr (Char.code piece.column + 2)
       || c = Char.chr (Char.code piece.column - 2))
  then true
  else false

(** [check_bishop_move piece c i] is a bool that checks if moving [piece] of 
  piece_type Bishop to row [r] and column [c] is legal or not. Returns true if 
    legal, false if not. *)
let check_bishop_move piece c i =
  if Char.code c - Char.code piece.column = i - piece.row then true else false

(** [check_rook_move piece c i] is a bool that checks if moving [piece] of 
  piece_type Rook to row [r] and column [c] is legal or not. Returns true if 
    legal, false if not. *)
let check_rook_move piece c i =
  if
    (Char.code c - Char.code piece.column = 0 && Int.abs (i - piece.row) > 0)
    || (i - piece.row = 0 && Int.abs (Char.code c - Char.code piece.column) > 0)
  then true
  else false

(** [check_queen_move piece c i] is a bool that checks if moving [piece] of 
  piece_type Queen to row [r] and column [c] is legal or not. Returns true if 
    legal, false if not. *)
let check_queen_move piece c i =
  if check_bishop_move piece c i && check_rook_move piece c i then true
  else false

(** [check_king_move piece c i] is a bool that checks if moving [piece] of 
  piece_type King to row [r] and column [c] is legal or not. Returns true if 
    legal, false if not. *)
let check_king_move piece c i =
  if
    c = Char.chr (Char.code piece.column)
    && (i = piece.row || i = piece.row + 1 || i = piece.row - 1)
    || c = Char.chr (Char.code piece.column + 1)
       && (i = piece.row || i = piece.row + 1 || i = piece.row - 1)
    || c = Char.chr (Char.code piece.column - 1)
       && (i = piece.row || i = piece.row + 1 || i = piece.row - 1)
  then true
  else false

(** [find_piece_type board piece c i] finds what type the [piece] is and calls 
function to check if moving [piece] to column [c] and row [i] is valid based on
its type. Returns true if legal move, false if not. *)
let find_piece_type board piece c i =
  match piece.piece_type with
  | Pawn -> check_pawn_move piece c i
  | Knight -> check_knight_move piece c i
  | Bishop -> check_bishop_move piece c i
  | Rook -> check_rook_move piece c i
  | Queen -> check_queen_move piece c i
  | King -> check_king_move piece c i

(** [check_valid_move_of_piece board piece c i] is a boolean that returns whether 
moving [piece] to column [c] and row [i] is a legal move or not. Returns true if 
the move is legal, and returns false if the move is not legal. Does not consider
other pieces that could be in the way of the move and also does not consider if 
the square on [r] and [c] is occupied *)
let rec check_piece_on_board (board : board) (piece : piece) (c : char)
    (i : int) : bool =
  match board with
  | [] -> false
  | h :: t
    when h.piece_type = piece.piece_type
         && h.color = piece.color && h.column = piece.column ->
      find_piece_type board piece c i
  | _ :: t -> check_piece_on_board t piece c i

let move board piece c i =
  if check_piece_on_board board piece c i then add_move_to_board board
