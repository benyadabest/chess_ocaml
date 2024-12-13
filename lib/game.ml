open Pieces
open Board

(* AF: A "game_state" represents the current state of the chess game. "board"
   represents the current position of pieces on the chessboard. "is_whites_move"
   indicates whether it is the White player's turn to move. "in_check" indicates
   whether the current player is in check. "game_over" indicates whether the
   game has ended from checkmate or stalemate.

   RI: The "board" must satisfy the RI defined in board.ml. "is_whites_move"
   must accurately reflect the player's turn. "in_check" must be true if and
   only if the current player's king is in check. "game_over" must be true if
   the position is in a checkmate or stalemate state. *)
   
type game_state = {
  board : board;
  is_whites_move : bool;
  in_check : bool;
  game_over : bool;
  error_message : string option;
}

let init_game () : game_state =
  {
    board = init_board ();
    is_whites_move = true;
    in_check = false;
    game_over = false;
    error_message = None;
  }

let get_board game_state = game_state.board
let game_over game_state = game_state.game_over
let is_whites_move game_state = game_state.is_whites_move

let current_player_color game_state =
  if game_state.is_whites_move then "White" else "Black"

let print_board (game_board : board) : unit =
  print_endline "";
  print_endline "";
  Board.print_board game_board;
  print_endline ""

let update_game_status (gs : game_state) : game_state =
  let color = current_player_color gs in
  let in_check = is_check gs.board color in
  let game_over = is_checkmate gs.board color in
  { gs with in_check; game_over }

let set_error_message (gs : game_state) (msg : string) : game_state =
  { gs with error_message = Some msg }

let clear_error_message (gs : game_state) : game_state =
  { gs with error_message = None }

let get_error_message (gs : game_state) : string option = gs.error_message

let move (game_state : game_state) (start_row, start_col) (end_row, end_col) :
    game_state =
  match get_piece_at game_state.board (start_row, start_col) with
  | None -> failwith "Internal error: No piece at that square."
  | Some piece ->
      let piece_color = get_color piece in
      if
        (piece_color = "White" && not (is_whites_move game_state))
        || (piece_color = "Black" && is_whites_move game_state)
      then failwith "Internal error: Move called on wrong player's turn."
      else if
        not
          (is_valid_move game_state.board (start_row, start_col)
             (end_row, end_col))
      then failwith "Internal error: Invalid move checked too late."
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
