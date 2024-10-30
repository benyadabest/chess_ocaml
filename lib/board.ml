open Pieces

(* Define the type for a square: either contains a piece or is empty *)
type square = piece option

type board = square array array


let init_board () : board =
  let board = Array.make_matrix 8 8 None in
  for col = 0 to 7 do
    board.(1).(col) <- Some { piece_type = Pawn; color = White };
    board.(6).(col) <- Some { piece_type = Pawn; color = Black }
  done;
  board

let get_piece_at (board : board) (row, col) = board.(row).(col)

let move_piece (board : board) (start_row, start_col) (end_row, end_col) : board = let piece = get_piece_at board (start_row, start_col) in
board.(start_row).(start_col) <- None;
board.(end_row).(end_col) <- piece;
board
