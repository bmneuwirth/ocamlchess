open Board

let main () =
  print_endline "\nWelcome to Chess 2.";
  let board = init_board in
  print_board board

let () = main ()
