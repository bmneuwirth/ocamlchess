open Board

type state = { board : board; color : color; can_castle : bool }

let init_state = { board = Board.init_board; color = White; can_castle = true }

let move cur_state start_col start_row end_col end_row =
  let { board; color = cur_color; _ } = cur_state in
  match Board.get_piece_color board start_col start_row with
  | Some c -> (
      if c <> cur_color then None
      else
        match Board.move board start_col start_row end_col end_row with
        | Some board ->
            Some
              {
                cur_state with
                board;
                color = (if cur_color = Black then White else Black);
              }
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
