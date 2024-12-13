open Chess

(* Helper function to execute moves and print the board after each move *)
let execute_moves_and_print board moves =
  List.fold_left
    (fun board (from_pos, to_pos) ->
      let board = Board.move_piece_algebraic board from_pos to_pos in
      Board.print_board board;
      print_endline "------------";
      board)
    board
    moves

(* Helper function to check board state and print results *)
let check_board_state board color =
  if Board.is_checkmate board color then
    print_endline "Checkmate detected!"
  else if Board.is_stalemate board color then
    print_endline "Stalemate detected!"
  else
    print_endline "No special condition detected."

(* Scholar's Mate *)
let test_scholars_mate () =
  let board = Board.init_board () in
  print_endline "Starting Scholar's Mate...";
  let board = execute_moves_and_print board [
    ("e2", "e4");
    ("e7", "e5");
    ("d1", "h5");
    ("b8", "c6");
    ("f1", "c4");
    ("h5", "f7") (* Checkmate *)
  ] in
  check_board_state board "Black"

(* Two-Move Mate *)
let test_two_move_mate () =
  let board = Board.init_board () in
  print_endline "Starting Two-Move Mate...";
  let board = execute_moves_and_print board [
    ("f2", "f3");
    ("e7", "e5");
    ("g2", "g4");
    ("d8", "h4") (* Checkmate *)
  ] in
  check_board_state board "White"

(* Ruy Lopez *)
let test_ruy_lopez () =
  let board = Board.init_board () in
  print_endline "Displaying capture, promotion, castling, and movement of all pieces!";
  let board = execute_moves_and_print board [
    ("e2", "e4");
    ("a7", "a5");
    ("g1", "f3");
    ("a5", "a4");
    ("f1", "c4");
    ("a4", "a3");
    ("e1", "g1");
    ("a3", "b2");
    ("f1", "e1");
    ("b2", "a1");
    ("c2", "c3");
    ("a1", "b1");
  ] in
  check_board_state board "White"

(* Custom Stalemate Position *)
let test_stalemate_position () =
  let board = Board.init_board () in
  (* Clear the board *)
  Array.iter (fun row -> Array.fill row 0 8 None) board;

  print_endline "Setting up stalemate position...";
  Board.place_piece board "a8" (Pieces.create_piece "King" "Black");
  Board.place_piece board "b6" (Pieces.create_piece "Pawn" "White");
  Board.place_piece board "c7" (Pieces.create_piece "King" "White");
  
  Board.print_board board;
  check_board_state board "Black"

(* Main Test Function *)
let () =
  test_scholars_mate ();
  print_endline "========================";
  test_two_move_mate ();
  print_endline "========================";
  test_ruy_lopez ();
  print_endline "========================";
  test_stalemate_position ();
