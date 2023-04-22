open Board

type player_state = { can_castle_left : bool; can_castle_right : bool }

type state = {
  board : board;
  color : color;
  white_state : player_state;
  black_state : player_state;
}

let init_player_state = { can_castle_left = true; can_castle_right = true }

let init_state =
  {
    board = Board.init_board;
    color = White;
    white_state = init_player_state;
    black_state = init_player_state;
  }

let move cur_state start_col start_row end_col end_row =
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
        | Some board ->
            Some
              (if cur_color = Black then
               {
                 cur_state with
                 board;
                 color = White;
                 black_state =
                   {
                     can_castle_left =
                       black_state.can_castle_left && piece.piece_type <> King
                       && not (piece.piece_type = Rook && start_col = 'A');
                     can_castle_right =
                       black_state.can_castle_right && piece.piece_type <> King
                       && not (piece.piece_type = Rook && start_col = 'H');
                   };
               }
              else
                {
                  cur_state with
                  board;
                  color = Black;
                  white_state =
                    {
                      can_castle_left =
                        white_state.can_castle_left && piece.piece_type <> King
                        && not (piece.piece_type = Rook && start_col = 'A');
                      can_castle_right =
                        white_state.can_castle_right && piece.piece_type <> King
                        && not (piece.piece_type = Rook && start_col = 'H');
                    };
                })
        | None -> None)
  | None -> None

let print_command { board; color; _ } =
  let _ =
    match color with
    | White -> ANSITerminal.print_string [ ANSITerminal.white ] "White's turn. "
    | Black -> ANSITerminal.print_string [ ANSITerminal.black ] "Black's turn. "
  in
  print_endline
    "Input your command: [move [start position] [end position]] or [quit]. Ex: \
     [move d2 d3]"
(* let remove_piece board piece = List.filter (fun x -> x != piece) board

   let update_piece piece col row = {piece.piece_type; piece.color; col; row}

   let update_state board piece col row = remove_piece board piece @ (update_piece piece col row) *)
