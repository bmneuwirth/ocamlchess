exception Invalid
(** Raised when an invalid command is parsed. *)

exception Empty
(** Raised when an empty command is parsed. *)

type command_data = {
  start_col : char;
  start_row : int;
  end_col : char;
  end_row : int;
}
(** The type [command_data] represents the command data phrase the can be part 
    of a player command. *)

(** The type [command] represents a player command that is decomposed into a 
verb and possibly a [command_data] or [Board.piece_type]. Invariant: the 
[command_data] carried by [Move] and the [Board.piece_type] 
carried by [Promote] must not be empty. *)
type command = Quit | Move of command_data | Promote of Board.piece_type

val parse : string -> command
(** [parse str] parses a player's input into a [command]. The first word 
(i.e., consecutive sequence of non-space characters) of [str] becomes the verb. 
The rest of the words, if any, become the command_data or Board.piece_type.

Requires: [str] contians only alphanumeric (A-Z, a-z, 0-9) and space characters.

Raises: [Empty] is [str] is the empty string or contains only spaces.

Raises: [Invalid] if the command is invalid. A command is invalid if the verb is 
neither "quit", "move" nor "promote", or if the verb is "quit" and there is a
phrase that follows it, or if the verb is "move" or "promote" and there is no 
phrase that follows it. *)
