exception King_not_found
exception CheckMate

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

let string_of_list ?(open_delim = "[") ?(close_delim = "]") ?(sep = "; ")
    string_of_elt lst =
  let len = List.length lst in
  let open Buffer in
  let buf = create (3 * len) in
  add_string buf open_delim;
  List.iteri
    (fun i v ->
      add_string buf (string_of_elt v);
      if i < len - 1 then add_string buf sep)
    lst;
  add_string buf close_delim;
  contents buf

let to_string_pair (p : char * int) : string =
  "(" ^ (fst p |> Char.escaped) ^ ", " ^ (snd p |> Int.to_string) ^ ")"

let make_piece piece_type color column row = { piece_type; color; column; row }

let init_pieces color : board =
  let pawn_start = if color = White then 2 else 7 in
  let back_start = if color = White then 1 else 8 in
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

let printing_board b =
  let _ =
    List.map
      (fun x -> print_endline (Char.escaped x.column ^ string_of_int x.row))
      b
  in
  ()

let init_board = init_pieces White @ init_pieces Black

(** [col_int_to_char col] returns the character representing the [col]th column. *)
let col_int_to_char col = Char.chr (col + Char.code 'A' - 1)

(** [col_char_to_int col] returns the int representing column [col]. *)
let col_char_to_int col = Char.code col - Char.code 'A' + 1

(** [piece_type_to_char p] returns the character representing a piece type [p]. *)
let piece_type_to_char p =
  match p with
  | Pawn -> 'P'
  | Rook -> 'R'
  | Knight -> 'N'
  | Bishop -> 'B'
  | Queen -> 'Q'
  | King -> 'K'

let get_piece b col row =
  List.find_opt (fun x -> x.column = col && x.row = row) b

let piece_exists b col row =
  match get_piece b col row with Some piece -> true | None -> false

let get_piece_color b col row =
  match get_piece b col row with Some p -> Some p.color | None -> None

let remove_piece b col row =
  List.filter (fun x -> not (x.column = col && x.row = row)) b

  let contents (opt : board option) = match opt with Some x -> x | None -> []
(** [print_piece_char b col row] prints the character representing the piece on
   board b at row r and col c, where row and col are both ints. *)
let print_piece_char b col row =
  let piece_opt = get_piece b col row in
  match piece_opt with
  | None -> print_char '.'
  | Some piece ->
      let color =
        if piece.color = White then [ ANSITerminal.white ]
        else [ ANSITerminal.black ]
      in
      ANSITerminal.print_string color
        (String.make 1 (piece_type_to_char piece.piece_type))

let print_board b =
  print_char '\n';
  for row = board_height downto 1 do
    ANSITerminal.print_string [ ANSITerminal.green ] (string_of_int row);
    print_char ' ';
    for col = 1 to board_width do
      print_piece_char b (col_int_to_char col) row;
      if col <> board_width then print_char ' ' else ()
    done;
    print_endline ""
  done;
  print_string "  ";
  for col = 1 to board_width do
    ANSITerminal.print_string [ ANSITerminal.green ]
      (String.make 1 (col_int_to_char col));
    if col <> board_width then print_char ' ' else ()
  done;
  print_endline ""

(** [next_col col] gets the column to the right of [col]. *)
let next_col col = col_int_to_char (col_char_to_int col + 1)

(** [prev_col col] gets the column to the left of [col]. *)
let prev_col col = col_int_to_char (col_char_to_int col - 1)

let piece_exists_on_right_diagonal piece b c i = piece_exists b c i

let piece_exists_on_left_diagonal piece b c i =
  piece_exists b (col_int_to_char (col_char_to_int piece.column - 1)) i

let white_pawn_movement piece b c i =
  match col_char_to_int piece.column - col_char_to_int c with
  | 0 -> i = piece.row + 1 || (piece.row = 2 && i = piece.row + 2)
  | -1 ->
      let is_piece_on_dest = piece_exists b c i in
      is_piece_on_dest
      && i = piece.row + 1
      && c = col_int_to_char (col_char_to_int piece.column + 1)
  | 1 ->
      let is_piece_on_dest = piece_exists b c i in
      is_piece_on_dest
      && i = piece.row + 1
      && c = col_int_to_char (col_char_to_int piece.column - 1)
  | _ -> false

let black_pawn_movement piece b c i =
  match col_char_to_int piece.column - col_char_to_int c with
  | 0 ->
      i = piece.row - 1
      || (piece.row = 7 && i = piece.row - 2 && c = piece.column)
  | -1 ->
      let is_piece_on_dest = piece_exists b c i in
      is_piece_on_dest
      && i = piece.row - 1
      && c = col_int_to_char (col_char_to_int piece.column + 1)
  | 1 ->
      let is_piece_on_dest = piece_exists b c i in
      is_piece_on_dest
      && i = piece.row - 1
      && c = col_int_to_char (col_char_to_int piece.column - 1)
  | _ -> false

(** [check_pawn_move piece b c i] is a bool that checks if moving [piece] of 
  piece_type Pawn to row [r] and column [c] on board [b] is legal or not. Returns true if 
    legal, false if not. *)
let check_pawn_move piece b c i =
  match piece.color with
  | White -> white_pawn_movement piece b c i
  | Black -> black_pawn_movement piece b c i

let check_pawn_end_pos piece c i =
  piece.color = White && c = piece.column
  && (i = piece.row + 1 || (piece.row = 2 && i = piece.row + 2))
  || piece.color = Black && c = piece.column
     && (i = piece.row - 1 || (piece.row = 7 && i = piece.row - 2))

let check_knight_end_pos piece c i =
  i = piece.row + 1
  && abs (col_char_to_int c - col_char_to_int piece.column) = 2
  || i = piece.row - 1
     && abs (col_char_to_int c - col_char_to_int piece.column) = 2
  || col_char_to_int c = col_char_to_int piece.column + 1
     && (i = piece.row + 2 || i = piece.row - 2)
  || col_char_to_int c = col_char_to_int piece.column - 1
     && (i = piece.row + 2 || i = piece.row - 2)

let check_bishop_end_pos piece c i =
  abs (col_char_to_int c - col_char_to_int piece.column) = abs (i - piece.row)

let check_rook_end_pos piece c i =
  (c = piece.column && Int.abs (i - piece.row) > 0)
  || i = piece.row
     && Int.abs (col_char_to_int c - col_char_to_int piece.column) > 0

let check_queen_end_pos piece c i =
  check_bishop_end_pos piece c i || check_rook_end_pos piece c i

let check_king_end_pos piece c i =
  (c = piece.column && (i = piece.row + 1 || i = piece.row - 1))
  || c = next_col piece.column
     && (i = piece.row || i = piece.row + 1 || i = piece.row - 1)
  || c = prev_col piece.column
     && (i = piece.row || i = piece.row + 1 || i = piece.row - 1)

let check_if_occupied_Jack (board : board) (c : char) (i : int) (color : color)
    : bool =
  match get_piece board c i with
  | Some piece when piece.color = color -> true
  | _ -> false

(** [next_square piece (start_col, start_row) (end_col, end_row)] returns the 
      next square in the path from [(start_col, start_row)] to [(end_col, end_row)] 
      depending on the piece_type of [piece]. *)
let check_if_occupied (board : board) (c : char) (i : int) : bool =
  match get_piece board c i with Some piece -> true | None -> false

let next_square piece (start_col, start_row) (end_col, end_row) =
  match piece.piece_type with
  | Pawn ->
      if piece.color = White then (start_col, start_row + 1)
      else (start_col, start_row - 1)
  | Bishop ->
      if end_col < start_col then
        if end_row < start_row then (prev_col start_col, start_row - 1)
        else (prev_col start_col, start_row + 1)
      else if end_row < start_row then (next_col start_col, start_row - 1)
      else (next_col start_col, start_row + 1)
  | Rook ->
      if end_col = start_col then
        if end_row < start_row then (start_col, start_row - 1)
        else (start_col, start_row + 1)
      else if end_col < start_col then (prev_col start_col, start_row)
      else (next_col start_col, start_row)
  | Queen ->
      if end_col = start_col then
        if end_row < start_row then (start_col, start_row - 1)
        else (start_col, start_row + 1)
      else if end_row = start_row then
        if end_col < start_col then (prev_col start_col, start_row)
        else (next_col start_col, start_row)
      else if end_col < start_col then
        if end_row < start_row then (prev_col start_col, start_row - 1)
        else (prev_col start_col, start_row + 1)
      else if end_row < start_row then (next_col start_col, start_row - 1)
      else (next_col start_col, start_row + 1)
  | King -> failwith "Should never occur"
  | Knight -> failwith "Should never occur"

(** [find_path board piece (start_col, start_row) (end_col, end_row)] returns 
  the list of squares that would be traveled in the path of [piece] from 
  [(start_col, start_row)] to [(end_col, end_row)].*)

let rec find_path board piece (start_col, start_row) (end_col, end_row) =
  match (start_col, start_row) with
  | x, y when x = end_col && y = end_row -> []
  | _ ->
      (start_col, start_row)
      :: find_path board piece
           (next_square piece (start_col, start_row) (end_col, end_row))
           (end_col, end_row)

(** [check_each_square board lst] checks every square in list, returns true if
  every square is not occupied, returns false if at least one square is 
    occupied. *)
let rec check_each_square board lst =
  match lst with
  | [] -> true
  | h :: t ->
      if check_if_occupied board (fst h) (snd h) then false
      else check_each_square board t

let check_btwn_squares board piece c i =
  check_each_square board
    (find_path board piece
       (next_square piece (piece.column, piece.row) (c, i))
       (c, i))

let check_valid_move board piece c i =
  if col_char_to_int c > 8 || col_char_to_int c < 1 || i > 8 || i < 1 then false
  else
    match piece.piece_type with
    | Pawn ->
        let one_step_only = Int.abs (piece.row - i) <= 1 in
        check_pawn_move piece board c i
        && (one_step_only || check_btwn_squares board piece c i)
    | Bishop ->
        check_bishop_end_pos piece c i && check_btwn_squares board piece c i
    | Rook -> check_rook_end_pos piece c i && check_btwn_squares board piece c i
    | Queen ->
        check_queen_end_pos piece c i && check_btwn_squares board piece c i
    | Knight -> check_knight_end_pos piece c i
    | King -> check_king_end_pos piece c i

let rec check_valid_piece_on_board (oboard : board) (board : board)
    (piece : piece) (c : char) (i : int) : bool =
  match board with
  | [] -> false
  | h :: t
    when h.piece_type = piece.piece_type
         && h.color = piece.color && h.column = piece.column
         && h.row = piece.row ->
      check_valid_move oboard piece c i
  | _ :: t -> check_valid_piece_on_board oboard t piece c i

let try_castle (board : board) (piece : piece) (col : char) (row : int)
    (is_left : bool) : board option =
  let rook_pos = if is_left then 'A' else 'H' in
  let rook_dest = if is_left then 'D' else 'F' in
  let initial_king_row = if piece.color = White then 1 else 8 in
  let rook = Option.get (get_piece board rook_pos initial_king_row) in
  if
    check_valid_piece_on_board board board rook rook_dest initial_king_row
    && not (check_if_occupied board rook_dest initial_king_row)
  then
    let board_without_rook = remove_piece board rook_pos initial_king_row in
    let board_without_pieces =
      remove_piece board_without_rook 'E' initial_king_row
    in
    let new_king = { piece with column = col; row } in
    let new_rook = { rook with column = rook_dest; row } in
    Some (new_king :: new_rook :: board_without_pieces)
  else None

(** [promote] returns a board that contains the updated piece with type [promote_to_piece_type]. *)
let promote (board : board) (promote_to_piece_type : piece_type) =
  let pawn_to_promote =
    List.find (fun x -> x.piece_type = Pawn && (x.row = 8 || x.row = 1)) board
  in
  let board_without_pawn =
    remove_piece board pawn_to_promote.column pawn_to_promote.row
  in
  let new_piece = { pawn_to_promote with piece_type = promote_to_piece_type } in
  new_piece :: board_without_pawn

(** let updated_piece =
      if piece.piece_type = Pawn && abs (piece.row - row) = 2 then
        { piece with en_passant_eligble = true }
      else piece
    in
    update_board board updated_piece col row*)

(* TODO: Add exception type for invalid moves? *)
(* TODO: Castling needs check checker to make sure it's a valid move (check
   if king is in check on each step of the castle)*)

(** [get_king board color] returns the the [color] King piece *)

let rec get_king (board : board) (color : color) =
  match board with
  | h :: t ->
      if h.piece_type = King && h.color = color then h else get_king t color
  | [] -> raise King_not_found

let print_color (c : color) : string =
  match c with Black -> "black" | White -> "white"

let rec checked (oboard : board) (board : board) (color : color)
    ((col, row) : char * int) =
  match board with
  | [] -> false
  | h :: t ->
      if
        h.color != color
        &&
        let f = check_valid_move oboard h col row in
        let _ =
          print_endline
            (string_of_bool f ^ print_color color ^ print_color h.color
           ^ Char.escaped h.column ^ string_of_int h.row ^ Char.escaped col
           ^ string_of_int row)
        in
        f
      then true
      else checked oboard t color (col, row)

let is_check (board : board) (color : color) =
  try
    let k = get_king board color in
    checked board board color (k.column, k.row)
  with King_not_found -> true

(** [get_k_moves board color (col,row) ] returns a list of valid moves for the [color] king at the positon (col,row) *)
let get_k_moves (board : board) (color : color) ((col, row) : char * int)
    (res : (char * int) list) =
  let king = get_king board color in
  let res =
    if
      check_valid_move board king col (row + 1)
      && not (check_if_occupied_Jack board col (row + 1) color)
    then res @ [ (col, row + 1) ]
    else res
  in
  let res =
    if
      check_valid_move board king col (row - 1)
      && not (check_if_occupied_Jack board col (row - 1) color)
    then res @ [ (col, row - 1) ]
    else res
  in
  let res =
    if
      check_valid_move board king (prev_col col) (row + 1)
      && not (check_if_occupied_Jack board (prev_col col) (row + 1) color)
    then res @ [ (prev_col col, row + 1) ]
    else res
  in
  let res =
    if
      check_valid_move board king (prev_col col) (row - 1)
      && not (check_if_occupied_Jack board (prev_col col) (row - 1) color)
    then res @ [ (prev_col col, row - 1) ]
    else res
  in
  let res =
    if
      check_valid_move board king (prev_col col) row
      && not (check_if_occupied_Jack board (prev_col col) row color)
    then res @ [ (prev_col col, row) ]
    else res
  in
  let res =
    if
      check_valid_move board king (next_col col) (row + 1)
      && not (check_if_occupied_Jack board (next_col col) (row + 1) color)
    then res @ [ (next_col col, row + 1) ]
    else res
  in
  let res =
    if
      check_valid_move board king (next_col col) (row - 1)
      && not (check_if_occupied_Jack board (next_col col) (row - 1) color)
    then res @ [ (next_col col, row - 1) ]
    else res
  in
  if
    check_valid_move board king (next_col col) row
    && not (check_if_occupied_Jack board (next_col col) row color)
  then res @ [ (next_col col, row) ]
  else res

let rec mated (moves : (char * int) list) (board : board) (color : color) =
  match moves with
  | [] -> true
  | h :: t -> if checked board board color h then mated t board color else false

let rec print_k_move lst acc =
  match lst with
  | (c, r) :: t -> print_k_move t acc ^ Char.escaped c ^ string_of_int r
  | [] -> acc

let all_pos =
  [
    ('A', 1);
    ('B', 1);
    ('C', 1);
    ('D', 1);
    ('E', 1);
    ('F', 1);
    ('G', 1);
    ('H', 1);
    ('A', 2);
    ('B', 2);
    ('C', 2);
    ('D', 2);
    ('E', 2);
    ('F', 2);
    ('G', 2);
    ('H', 2);
    ('A', 3);
    ('B', 3);
    ('C', 3);
    ('D', 3);
    ('E', 3);
    ('F', 3);
    ('G', 3);
    ('H', 3);
    ('A', 4);
    ('B', 4);
    ('C', 4);
    ('D', 4);
    ('E', 4);
    ('F', 4);
    ('G', 4);
    ('H', 4);
    ('A', 5);
    ('B', 5);
    ('C', 5);
    ('D', 5);
    ('E', 5);
    ('F', 5);
    ('G', 5);
    ('H', 5);
    ('A', 6);
    ('B', 6);
    ('C', 6);
    ('D', 6);
    ('E', 6);
    ('F', 6);
    ('G', 6);
    ('H', 6);
    ('A', 7);
    ('B', 7);
    ('C', 7);
    ('D', 7);
    ('E', 7);
    ('F', 7);
    ('G', 7);
    ('H', 7);
    ('A', 8);
    ('B', 8);
    ('C', 8);
    ('D', 8);
    ('E', 8);
    ('F', 8);
    ('G', 8);
    ('H', 8);
  ]

let rec get_all_open_pos board color pos =
  match pos with
  | (c, r) :: t ->
      if check_if_occupied_Jack board c r color then
        get_all_open_pos board color t
      else [ (c, r) ] @ get_all_open_pos board color t
  | [] -> []

let temp_board board piece col row color =
  let new_piece = { piece with column = col; row } in
  let board_without_piece = remove_piece board piece.column piece.row in
  match get_piece board col row with
  | None -> Some (new_piece :: board_without_piece)
  | Some captured_piece ->
      if captured_piece.color = piece.color then None
      else
        Some
          (new_piece
          :: remove_piece board_without_piece captured_piece.column
               captured_piece.row)

let rec is_any_mates board piece pos color =
  match pos with
  | (c, r) :: t ->
      if check_valid_move board piece c r then
        is_check (contents (temp_board board piece c r piece.color)) color
        && is_any_mates board piece t color
      else is_any_mates board piece t color
  | [] -> true

let rec mate_with_block oboard (board : board) color =
  let open_pos = get_all_open_pos oboard color all_pos in
  match board with
  | h :: t ->
      if h.color = color && h.piece_type != King then
        is_any_mates oboard h open_pos color && mate_with_block oboard t color
      else mate_with_block oboard t color
  | [] -> true

(** [is_mate board color (col,row)] returns a boolean on whether the [color] king is in checkmate *)
let is_mate (board : board) (color : color) =
  let c = if color = Black then White else Black in
  let k = get_king board c in
  mated
    (let i = get_k_moves board c (k.column, k.row) [ (k.column, k.row) ] in
     let _ = print_endline (print_k_move i "") in
     i)
    board c && mate_with_block board board c



let update_board board piece col row color =
  let new_piece = { piece with column = col; row } in
  let board_without_piece = remove_piece board piece.column piece.row in
  match get_piece board col row with
  | None -> (
      match is_mate (new_piece :: board_without_piece) color with
      | true ->
          let x = print_color color ^ " Wins" in
          let _ = print_endline x in
          raise CheckMate
      | false -> Some (new_piece :: board_without_piece))
  | Some captured_piece -> (
      if captured_piece.color = piece.color then None
      else
        match
          is_mate
            (new_piece
            :: remove_piece board_without_piece captured_piece.column
                 captured_piece.row)
            color
        with
        | true ->
            let x = print_color color ^ " Wins" in
            let _ = print_endline x in
            raise CheckMate
        | false ->
            Some
              (new_piece
              :: remove_piece board_without_piece captured_piece.column
                   captured_piece.row))

let move_piece (board : board) (piece : piece) (col : char) (row : int)
    (can_castle_left : bool) (can_castle_right : bool) : board option =
  let initial_king_row = if piece.color = White then 1 else 8 in
  let initial_king_move =
    piece.piece_type = King && piece.column = 'E'
    && piece.row = initial_king_row
  in
  if initial_king_move && col = 'C' && row = initial_king_row && can_castle_left
  then try_castle board piece col row true
  else if
    initial_king_move && col = 'G' && row = initial_king_row && can_castle_right
  then try_castle board piece col row false
  else if
    check_valid_piece_on_board board board piece col row
    && not
         (is_check
            (contents (update_board board piece col row piece.color))
            piece.color)
  then update_board board piece col row piece.color
  else None

(* TODO: Add exception type for invalid moves? *)
(* TODO: Castling needs check checker to make sure it's a valid move (check
   if king is in check on each step of the castle)*)

(** [get_king board color] returns the the [color] King piece *)
let move (board : board) (c1 : char) (i1 : int) (c2 : char) (i2 : int)
    (can_castle_left : bool) (can_castle_right : bool) : board option =
  match get_piece board c1 i1 with
  | Some p ->
      let piece = p in
      move_piece board piece c2 i2 can_castle_left can_castle_right
  | None -> None
