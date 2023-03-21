(** ["e4";"g5"] *)

exception Invalid
exception Empty

type command = {start_col : char; start_row : char; end_col : char; end_row : char}

let first_char_valid str = 
    if String.get str 0 |> String.contains "abcdefgh" = true then true else false

let second_char_valid str = 
    if String.get str 1 |> String.contains "12345678" = true then true else false

let first_char str = String.get str 0 
let second_char str = String.get str 1 

let parse str = 
    let spliced_strlst = String.split_on_char ' ' str in
    let no_emptystr_strlst = List.filter (fun x -> x <> "") spliced_strlst in 
    match no_emptystr_strlst with 
    | [] -> raise Empty 
    | h1 :: h2 :: _ -> 
        if first_char_valid h1 && second_char_valid h1 && first_char_valid h2 && second_char_valid h2 
            then {start_col = first_char h1; start_row = second_char h1; end_col = first_char h2; end_row = second_char h2} else raise Invalid 
    | _ :: _ -> raise Invalid

