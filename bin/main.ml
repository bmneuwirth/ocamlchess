type color = Black | White
type piece_info = color * char * int

type piece =
| Pawn of piece_info
| Knight of piece_info
| Bishop of piece_info
| Rook of piece_info
| Queen of piece_info
| King of piece_info

let init_pieces color = 
  let pawn_start = if color = White then 7 else 2 in
  let back_start = if color = White then 8 else 1 in
  [Pawn (color, 'A', pawn_start); Pawn (color, 'B', pawn_start); Pawn (color, 'C', pawn_start); 
  Pawn (color, 'D', pawn_start); Pawn (color, 'E', pawn_start); Pawn (color, 'F', pawn_start);
  Pawn (color, 'G', pawn_start); Pawn (color, 'H', pawn_start);
  Rook (color, 'A', back_start); Rook (color, 'H', back_start); Knight (color, 'B', back_start);
  Knight (color, 'G', back_start); Bishop (color, 'C', back_start); Bishop (color, 'F', back_start);
  Queen (color, 'D', back_start); King (color, 'E', back_start)
  ] 

let init_board = init_pieces White @ init_pieces Black

let print_board b = if b = [] then () else print_endline "board"

let main () = 
  print_endline "\nWelcome to Chess 2.";
  let board = init_board in print_board board
  
let () = main ()