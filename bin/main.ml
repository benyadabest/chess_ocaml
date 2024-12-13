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

let would_still_be_in_check game (start_row, start_col) (end_row, end_col) =
  let board = get_board game in
  try
    let temp_board =
      Chess.Board.move_piece board (start_row, start_col) (end_row, end_col)
    in
    let color = current_player_color game in
    Chess.Board.is_check temp_board color
  with Failure _ -> true

let filter_moves_to_avoid_check game start_pos moves =
  let board = get_board game in
  let color = current_player_color game in
  List.filter
    (fun (end_row, end_col) ->
      try
        let temp_board =
          Chess.Board.move_piece board start_pos (end_row, end_col)
        in
        not (Chess.Board.is_check temp_board color)
      with Failure _ -> false)
    moves

let get_valid_moves_for_piece game piece start_pos =
  let board = get_board game in
  let moves =
    Chess.Board.valid_piece_move board piece (fst start_pos, snd start_pos)
  in
  List.filter
    (fun (er, ec) -> not (would_still_be_in_check game start_pos (er, ec)))
    moves

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
  let board = get_board game in
  let color = current_player_color game in
  let currently_in_check = Chess.Board.is_check board color in

  let start_coords =
    read_algebraic
      "Enter the coordinates of the piece you want to move (e.g. 'e2'): "
  in
  let piece_opt = Chess.Board.get_piece_at board start_coords in
  match piece_opt with
  | None ->
      let game = set_error_message game "No piece at that square!" in
      perform_move game
  | Some piece ->
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
        let moves = get_valid_moves_for_piece game piece start_coords in

        let moves =
          if currently_in_check then
            filter_moves_to_avoid_check game start_coords moves
          else moves
        in

        if moves = [] then
          let game =
            set_error_message game
              (if currently_in_check then
                 "You are in check and have no legal moves for this piece!"
               else "No legal moves for this piece!")
          in
          perform_move game
        else begin
          let end_coords =
            read_algebraic
              "Enter the coordinates to move this piece to (e.g. 'e4'): "
          in
          if not (List.mem end_coords moves) then
            let game = set_error_message game "That move is not allowed!" in
            perform_move game
          else
            try
              let new_game = move game start_coords end_coords in
              print_endline "Move successful!";
              new_game
            with Failure msg ->
              let new_game =
                set_error_message game "Invalid move for that piece."
              in
              perform_move new_game
        end

let rec game_loop game =
  if game_over game then begin
    let color = if is_whites_move game then "White" else "Black" in
    clear_screen ();
    if Chess.Board.is_checkmate (get_board game) color then
      let winner = if is_whites_move game then "Black" else "White" in
      printf "Checkmate! %s wins!\n" winner
    else print_endline "Game over!"
  end
  else begin
    let new_game = perform_move game in
    if game_over new_game then begin
      let color = if is_whites_move new_game then "White" else "Black" in
      clear_screen ();
      if Chess.Board.is_checkmate (get_board new_game) color then
        let winner = if is_whites_move new_game then "Black" else "White" in
        printf "Checkmate! %s wins!\n" winner
      else print_endline "Game over!"
    end
    else game_loop new_game
  end

let () =
  let game = init_game () in
  game_loop game
