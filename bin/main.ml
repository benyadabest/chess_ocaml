open Chess.Game
open Printf

let read_coords prompt =
  print_string prompt;
  flush stdout;
  let input = read_line () in
  try
    let r, c = Scanf.sscanf input "%d %d" (fun r c -> (r, c)) in
    (r, c)
  with _ ->
    print_endline "Invalid input. Please enter two integers like '1 4'.";
    read_coords prompt

let rec perform_move game =
  let color_str = if is_whites_move game then "White" else "Black" in
  printf "%s's turn.\n" color_str;
  print_board game;

  let start_coords =
    read_coords
      "Enter the coordinates of the piece you want to move (e.g. '1 4'): "
  in
  let piece_opt = Chess.Board.get_piece_at game.board start_coords in
  match piece_opt with
  | None ->
      print_endline "No piece at that square!";
      perform_move game
  | Some piece -> (
      let piece_color = Chess.Pieces.get_color piece in
      if
        (is_whites_move game && piece_color <> "White")
        || ((not (is_whites_move game)) && piece_color <> "Black")
      then begin
        print_endline "That piece does not belong to you!";
        perform_move game
      end
      else
        let end_coords =
          read_coords
            "Enter the coordinates to move this piece to (e.g. '3 4'): "
        in
        try
          let new_game = move game start_coords end_coords in
          print_endline "Move successful!";
          new_game
        with Failure msg ->
          print_endline msg;
          perform_move game

let rec game_loop game =
  if game_over game then begin
    if
      Board.is_checkmate game.board
        (if is_whites_move game then White else Black)
    then
      let winner = if is_whites_move game then "Black" else "White" in
      printf "Checkmate! %s wins!\n" winner
    else if
      Board.is_stalemate game.board
        (if is_whites_move game then White else Black)
    then print_endline "Stalemate! It's a draw!"
    else print_endline "Game over!"
  end
  else
    let new_game = perform_move game in
    game_loop new_game

let () =
  let game = init_game () in
  game_loop game
