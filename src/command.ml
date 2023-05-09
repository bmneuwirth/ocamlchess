exception Invalid
exception Empty

type command_data = {
  start_col : char;
  start_row : int;
  end_col : char;
  end_row : int;
}

type command = Quit | Move of command_data | Promote of Board.piece_type

let promote_char_valid str =
  if String.get str 0 |> String.contains "qrbnQRBN" then true else false

let first_char_valid str =
  if String.get str 0 |> String.contains "abcdefghABCDEFGH" = true then true
  else false

let second_char_valid str =
  if String.get str 1 |> String.contains "12345678" = true then true else false

let first_char str = Char.uppercase_ascii (String.get str 0)
let second_char str = Char.code (String.get str 1) - Char.code '0'

let parse str =
  let spliced_strlst = String.split_on_char ' ' str in
  let no_emptystr_strlst = List.filter (fun x -> x <> "") spliced_strlst in
  match no_emptystr_strlst with
  | [] -> raise Empty
  | [ "quit" ] -> Quit
  | "move" :: h1 :: h2 :: _ ->
      if
        String.length h1 > 1
        && String.length h2 > 1
        && first_char_valid h1 && second_char_valid h1 && first_char_valid h2
        && second_char_valid h2
      then
        Move
          {
            start_col = first_char h1;
            start_row = second_char h1;
            end_col = first_char h2;
            end_row = second_char h2;
          }
      else raise Invalid
  | "promote" :: h1 :: _ ->
      if String.length h1 > 0 && promote_char_valid h1 then
        match h1 with
        | "q" -> Promote Queen
        | "Q" -> Promote Queen
        | "b" -> Promote Bishop
        | "B" -> Promote Bishop
        | "r" -> Promote Rook
        | "R" -> Promote Rook
        | "n" -> Promote Knight
        | "N" -> Promote Knight
        | _ -> raise Invalid
      else raise Invalid
  | _ :: _ -> raise Invalid
