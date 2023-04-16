(** [game_loop state] runs the game loop with current state [state] until the command [quit] is received. *)
let rec game_loop (board, color) =
  match Command.parse (read_line ()) with
  | exception Command.Empty -> print_status (board, color) "Invalid command.\n"
  | exception Command.Invalid ->
      print_status (board, color) "Invalid command.\n"
  | Quit ->
      print_endline "Thanks for playing!\n";
      exit 0
  | Move { start_col; start_row; end_col; end_row } -> (
      let new_state_opt =
        State.move (board, color) start_col start_row end_col end_row
      in
      match new_state_opt with
      | None -> print_status (board, color) "Invalid move.\n"
      | Some state -> print_status state "")

(** [print_status state msg] prints the current status of the game using the 
    state and an optional additional message [msg]. *)
and print_status (board, color) msg =
  Board.print_board board;
  if msg <> "" then ANSITerminal.print_string [ ANSITerminal.red ] msg;
  State.print_command (board, color);
  game_loop (board, color)

let main () =
  let state = State.init_state in
  print_status state "Welcome to Chess 2!\n"

let () = main ()
