open Pieces
open Board

type game_state = {
  board : board;
  is_whites_move : bool;
  in_check : bool;
  game_over : bool;
}

let init_game () : game_state =
  {
    board = init_board ();
    is_whites_move = true;
    in_check = false;
    game_over = false;
  }

let game_over game_state = game_state.game_over
let is_whites_move game_state = game_state.is_whites_move

let current_player_color game_state =
  if game_state.is_whites_move then "White" else "Black"

let print_board (game_state : game_state) : unit = print_board game_state.board

let update_game_status (gs : game_state) : game_state =
  let color = current_player_color gs in
  let in_check = is_in_check gs.board color in
  let game_over = is_checkmate gs.board color || is_stalemate gs.board color in
  { gs with in_check; game_over }

let move (game_state : game_state) (start_row, start_col) (end_row, end_col) :
    game_state =
  let piece_opt = get_piece_at game_state.board (start_row, start_col) in
  match piece_opt with
  | None -> failwith "No piece at that square."
  | Some piece ->
      let piece_color = get_color piece in
      if
        (piece_color = "White" && not (is_whites_move game_state))
        || (piece_color = "Black" && is_whites_move game_state)
      then failwith "It's not your turn to move."
      else if
        not
          (is_valid_move game_state.board (start_row, start_col)
             (end_row, end_col))
      then failwith "Invalid move for that piece."
      else
        let new_board =
          move_piece game_state.board (start_row, start_col) (end_row, end_col)
        in
        let new_game_state =
          {
            game_state with
            board = new_board;
            is_whites_move = not game_state.is_whites_move;
          }
        in
        update_game_status new_game_state
