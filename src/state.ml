open Board

type player_state = { can_castle_left : bool; can_castle_right : bool }

type state = {
  board : board;
  color : color;
  white_state : player_state;
  black_state : player_state;
  can_promote : bool;
}

let init_player_state = { can_castle_left = true; can_castle_right = true }

let init_state =
  {
    board = Board.init_board;
    color = White;
    white_state = init_player_state;
    black_state = init_player_state;
    can_promote = false;
  }

(** [update_state board cur_state piece] updates the state based on new board 
    [board], previous state [cur_state], original piece that was moved 
    [piece], and destination row of the move [end_row]*)
let update_state board cur_state piece end_row =
  let { color = cur_color; black_state; white_state } = cur_state in
  let gen_new_state new_color player_state =
    let new_state =
      {
        cur_state with
        board;
        color = new_color;
        can_promote = piece.piece_type = Pawn && (end_row = 1 || end_row = 8);
      }
    in
    let player_state =
      {
        can_castle_left =
          player_state.can_castle_left && piece.piece_type <> King
          && not (piece.piece_type = Rook && piece.column = 'A');
        can_castle_right =
          player_state.can_castle_right && piece.piece_type <> King
          && not (piece.piece_type = Rook && piece.column = 'H');
      }
    in
    if new_color = White then { new_state with black_state = player_state }
    else { new_state with white_state = player_state }
  in
  Some
    (if cur_color = Black then gen_new_state White black_state
    else gen_new_state Black white_state)

let move start_col start_row end_col end_row (cur_state : state) =
  let { board; color = cur_color; black_state; white_state } = cur_state in
  match Board.get_piece_color board start_col start_row with
  | Some c -> (
      if c <> cur_color then None
      else
        let piece = Option.get (Board.get_piece board start_col start_row) in
        let can_castle_left =
          if cur_color = White then white_state.can_castle_left
          else black_state.can_castle_left
        in
        let can_castle_right =
          if cur_color = White then white_state.can_castle_right
          else black_state.can_castle_right
        in
        match
          Board.move board start_col start_row end_col end_row can_castle_left
            can_castle_right
        with
        | Some board -> update_state board cur_state piece end_row
        | None -> None)
  | None -> None

let print_command { board; color; white_state; black_state; can_promote } =
  if can_promote then
    let _ =
      match color with
      | Black ->
          ANSITerminal.print_string [ ANSITerminal.white ] "White's turn. "
      | White ->
          ANSITerminal.print_string [ ANSITerminal.black ] "Black's turn. "
    in
    print_endline
      "Choose a piece type to promote the pawn to: [promote [piece type]] or \
       [quit]. Ex: [promote q]"
  else
    let _ =
      match color with
      | White ->
          ANSITerminal.print_string [ ANSITerminal.white ] "White's turn. "
      | Black ->
          ANSITerminal.print_string [ ANSITerminal.black ] "Black's turn. "
    in
    print_endline
      "Input your command: [move [start position] [end position]] or [quit]. \
       Ex: [move d2 d3]"

(* let remove_piece board piece = List.filter (fun x -> x != piece) board

   let update_piece piece col row = {piece.piece_type; piece.color; col; row}

   let update_state board piece col row = remove_piece board piece @ (update_piece piece col row) *)
