open Board

let remove_piece board piece = List.filter (fun x -> x != piece) board

let update_piece piece col row = {piece.piece_type; piece.color; col; row}

let update_state board piece col row = remove_piece board piece @ (update_piece piece col row)
