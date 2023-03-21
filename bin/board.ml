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

(* [col_int_to_char col] returns the character representing the [col]th column. *)
let col_int_to_char col = Char.chr (col + Char.code 'A' - 1)

(* [piece_type_to_char p] returns the character representing a piece type [p]. *)
let piece_type_to_char p =
  match p with
  | Pawn -> 'P'
  | Rook -> 'R'
  | Knight -> 'N'
  | Bishop -> 'B'
  | Queen -> 'Q'
  | King -> 'K'

(* [get_piece b row col] returns the piece on board [b] at row [r] and col [c],
   where row and col are both ints. *)
let get_piece b row col =
  List.find_opt (fun x -> x.column = col_int_to_char col && x.row = row) b

(* [get_piece_char b row col] returns the character representing the piece on
   board b at row r and col c, where row and col are both ints. *)
let get_piece_char b row col =
  let piece_opt = get_piece b row col in
  match piece_opt with
  | None -> '.'
  | Some piece -> piece_type_to_char piece.piece_type

let print_board b =
  for row = 1 to board_height do
    for col = 1 to board_width do
      print_char (get_piece_char b row col);
      if col <> board_width then print_char ' ' else ()
    done;
    print_endline ""
  done

let move board piece c i = raise (Failure "Unimplemented")
