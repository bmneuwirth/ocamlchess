exception Invalid
exception Empty

type command_data = {
  start_col : char;
  start_row : int;
  end_col : char;
  end_row : int;
}

type command = Quit | Move of command_data

val parse : string -> command
(** Parses the given string into a command. *)
