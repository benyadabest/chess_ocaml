open Chess.Game

let game = init_game ()
let () = print_endline "Initial Game:"
let () = print_board game
let () = print_endline "After White Pawn Move:"

let () =
  try
    let game = move game (1, 4) (3, 4) in
    print_board game
  with Failure msg -> print_endline msg

let () = print_endline "After Black Pawn Move:"

let () =
  try
    let game = move game (6, 4) (4, 4) in
    print_board game
  with Failure msg -> print_endline msg
