open Board

type state = board * color

let init_state = (Board.init_board, White)

let move (board, color) start_col start_row end_col end_row =
  match Board.get_piece_color board start_col start_row with
  | Some c -> (
      if c <> color then None
      else
        match Board.move board start_col start_row end_col end_row with
        | Some board -> Some (board, if color = Black then White else Black)
        | None -> None)
  | None -> None

let print_command (board, color) =
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
