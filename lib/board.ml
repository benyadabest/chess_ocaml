open Pieces

(* Define the type for a square: either contains a piece or is empty *)
type square = piece option
type board = square array array

let init_board () : board =
  let board = Array.make_matrix 8 8 None in
  board.(0).(0) <- Some (create_piece "Rook" "White");
  board.(7).(0) <- Some (create_piece "Rook" "Black");
  board.(0).(7) <- Some (create_piece "Rook" "White");
  board.(7).(7) <- Some (create_piece "Rook" "Black");
  board.(0).(1) <- Some (create_piece "Knight" "White");
  board.(7).(1) <- Some (create_piece "Knight" "Black");
  board.(0).(6) <- Some (create_piece "Knight" "White");
  board.(7).(6) <- Some (create_piece "Knight" "Black");
  board.(0).(2) <- Some (create_piece "Bishop" "White");
  board.(7).(2) <- Some (create_piece "Bishop" "Black");
  board.(0).(5) <- Some (create_piece "Bishop" "White");
  board.(7).(5) <- Some (create_piece "Bishop" "Black");
  board.(0).(4) <- Some (create_piece "King" "White");
  board.(7).(4) <- Some (create_piece "King" "Black");
  board.(0).(3) <- Some (create_piece "Queen" "White");
  board.(7).(3) <- Some (create_piece "Queen" "Black");
  for col = 0 to 7 do
    board.(1).(col) <- Some (create_piece "Pawn" "White");
    board.(6).(col) <- Some (create_piece "Pawn" "Black")
  done;
  board

let get_piece_at (board : board) (row, col) = board.(row).(col)

let move_piece (board : board) (start_row, start_col) (end_row, end_col) : board
    =
  let piece = get_piece_at board (start_row, start_col) in
  board.(start_row).(start_col) <- None;
  board.(end_row).(end_col) <- piece;
  board
