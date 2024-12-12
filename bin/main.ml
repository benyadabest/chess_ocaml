open Chess.Game
open Printf

let red = "\027[31m"
let reset = "\027[0m"

let clear_screen () =
  print_string "\027[2J\027[H";
  flush stdout

let rec read_algebraic prompt =
  print_string prompt;
  flush stdout;
  let input = read_line () in
  try
    let indices = Chess.Board.algebraic_to_indices input in
    indices
  with _ ->
    print_endline
      (red ^ "Invalid input. Please enter a position like 'e4'." ^ reset);
    read_algebraic prompt

let rec perform_move game =
  let color_str = if is_whites_move game then "White" else "Black" in

  clear_screen ();

  (match get_error_message game with
  | Some msg ->
      printf "%s%s%s %s's turn.\n" red msg reset color_str;
      let game = clear_error_message game in
      print_board (get_board game);
      game
  | None ->
      printf "%s's turn.\n" color_str;
      print_board (get_board game);
      game)
  |> fun game ->
  let start_coords =
    read_algebraic
      "Enter the coordinates of the piece you want to move (e.g. 'e2'): "
  in
  let piece_opt = Chess.Board.get_piece_at (get_board game) start_coords in
  match piece_opt with
  | None ->
      let game = set_error_message game "No piece at that square!" in
      perform_move game
  | Some piece -> (
      let piece_color = Chess.Pieces.get_color piece in
      if
        (is_whites_move game && piece_color <> "White")
        || ((not (is_whites_move game)) && piece_color <> "Black")
      then
        let game =
          set_error_message game "That piece does not belong to you!"
        in
        perform_move game
      else
        let end_coords =
          read_algebraic
            "Enter the coordinates to move this piece to (e.g. 'e4'): "
        in
        try
          let new_game = move game start_coords end_coords in
          print_endline "Move successful!";
          new_game
        with Failure msg ->
          let new_game =
            set_error_message game "Invalid move for that piece."
          in
          perform_move new_game)

let rec game_loop game =
  if game_over game then begin
    let color = if is_whites_move game then "White" else "Black" in
    clear_screen ();
    if Chess.Board.is_checkmate (get_board game) color then
      let winner = if is_whites_move game then "Black" else "White" in
      printf "Checkmate! %s wins!\n" winner
    else print_endline "Game over!"
  end
  else
    let new_game = perform_move game in
    game_loop new_game

let () =
  let game = init_game () in
  game_loop game
