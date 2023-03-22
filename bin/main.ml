open Board
open Command

(** [game_loop board] runs the game loop with current board [board] until the command [quit] is received. *)
let rec game_loop board =
  match Command.parse (read_line ()) with
  | exception Command.Empty -> print_status board "Invalid command.\n"
  | exception Command.Invalid -> print_status board "Invalid command.\n"
  | Quit ->
      print_endline "Thanks for playing!\n";
      exit 0
  | Move { start_col; start_row; end_col; end_row } -> (
      let new_board_opt = move board start_col start_row end_col end_row in
      match new_board_opt with
      | None -> print_status board "Invalid move.\n"
      | Some b -> print_status b "")

and print_status board msg =
  print_board board;
  if msg <> "" then ANSITerminal.print_string [ ANSITerminal.red ] msg;
  print_endline
    "Input your command: [move [start position] [end position]] or [quit]. Ex: \
     [move d2 d3]";
  game_loop board

let main () =
  let board = init_board in
  print_status board "Welcome to Chess 2!\n"

let () = main ()
