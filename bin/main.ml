open Chessmon
open State

(** [game_loop state] runs the game loop with current state [state] until the command [quit] is received. *)
let rec game_loop cur_state =
  match Command.parse (read_line ()) with
  | exception Command.Empty -> print_status cur_state "Invalid command.\n"
  | exception Command.Invalid -> print_status cur_state "Invalid command.\n"
  | Quit ->
      print_endline "Thanks for playing!\n";
      exit 0
  | Move { start_col; start_row; end_col; end_row } -> (
      let new_state_opt =
        State.move start_col start_row end_col end_row cur_state
      in
      match new_state_opt with
      | None -> print_status cur_state "Invalid move.\n"
      | Some state -> print_status state "")

(** [print_status state msg] prints the current status of the game using the 
    state and an optional additional message [msg]. *)
and print_status cur_state msg =
  let { board; color; _ } = cur_state in
  Board.print_board board;
  if msg <> "" then ANSITerminal.print_string [ ANSITerminal.red ] msg;
  State.print_command cur_state;
  game_loop cur_state

let main () =
  let state = State.init_state in
  print_status state "Welcome to Chess 2!\n"

let () = main ()
