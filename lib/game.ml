open Pieces
open Board
open Pieces

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

let make_move game_state start_pos end_pos end_state =
  let start_piece = Board.get_piece_at game_state start_pos in
  match start_piece with
  | Some piece -> (
      let color = piece.color in
      let piece_type = piece.piece_type in
      match piece_type with
      | King -> failwith "continue implementation"
      | Queen -> failwith "continue implementation"
      | Rook -> failwith "continue implementation"
      | Bishop -> failwith "continue implementation"
      | Knight -> failwith "continue implementation"
      | Pawn -> failwith "continue implementation")
  | None ->
      failwith
        "Error: Cannot make move because there is no piece at the selected \
         start position."

let game_over game_state = game_state.game_over
let is_whites_move game_state = game_state.is_whites_move
