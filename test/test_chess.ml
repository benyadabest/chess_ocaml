open OUnit2
open Chess

(* Equality function for pieces *)
let pieces_equal p1 p2 =
  match (p1, p2) with
  | (None, None) -> true
  | (Some piece1, Some piece2) ->
      Pieces.get_piece_type piece1 = Pieces.get_piece_type piece2 &&
      Pieces.get_color piece1 = Pieces.get_color piece2
  | _ -> false

(* Print the board for debugging *)
let print_board_debug board =
  Board.print_board board;
  print_endline ""

(* Debug wrapper for is_valid_move *)
let debug_is_valid_move board start_pos end_pos =
  let result = Board.is_valid_move board start_pos end_pos in
  Printf.printf "Validating move from %s to %s: %b\n"
    (Board.indices_to_algebraic start_pos)
    (Board.indices_to_algebraic end_pos)
    result;
  result

(* Test the initialization of the chessboard *)
let test_init_board _ =
  let board = Board.init_board () in
  (* Check major pieces *)
  assert_bool "Black Rook at (0, 0)" (pieces_equal (Board.get_piece_at board (0, 0)) (Some (Pieces.create_piece "Rook" "Black")));
  assert_bool "Black Knight at (0, 1)" (pieces_equal (Board.get_piece_at board (0, 1)) (Some (Pieces.create_piece "Knight" "Black")));
  assert_bool "Black King at (0, 4)" (pieces_equal (Board.get_piece_at board (0, 4)) (Some (Pieces.create_piece "King" "Black")));
  assert_bool "White Pawn at (6, 0)" (pieces_equal (Board.get_piece_at board (6, 0)) (Some (Pieces.create_piece "Pawn" "White")));
  assert_bool "White King at (7, 4)" (pieces_equal (Board.get_piece_at board (7, 4)) (Some (Pieces.create_piece "King" "White")));
  (* Ensure empty squares are indeed empty *)
  assert_bool "Empty square at (4, 4)" (pieces_equal (Board.get_piece_at board (4, 4)) None);
  assert_bool "Empty square at (5, 5)" (pieces_equal (Board.get_piece_at board (5, 5)) None)

(* Test valid position checking *)
let test_valid_position _ =
  assert_bool "Position (0, 0) should be valid" (Board.is_valid_position (0, 0));
  assert_bool "Position (7, 7) should be valid" (Board.is_valid_position (7, 7));
  assert_bool "Position (-1, 0) should be invalid" (not (Board.is_valid_position (-1, 0)));
  assert_bool "Position (8, 0) should be invalid" (not (Board.is_valid_position (8, 0)))

(* Test move validity *)
let test_valid_move _ =
  let board = Board.init_board () in
  print_endline "Initial Board State:";
  print_board_debug board;

  (* White Pawn movement *)
  assert_bool "White pawn can move forward one step" (debug_is_valid_move board (6, 0) (5, 0));
  assert_bool "White pawn can move forward two steps initially" (debug_is_valid_move board (6, 0) (4, 0));
  assert_bool "White pawn cannot move forward three steps" (not (debug_is_valid_move board (6, 0) (3, 0)));
  
  (* Black Pawn movement *)
  assert_bool "Black pawn can move forward one step" (debug_is_valid_move board (1, 0) (2, 0));
  assert_bool "Black pawn can move forward two steps initially" (debug_is_valid_move board (1, 0) (3, 0));
  assert_bool "Black pawn cannot move forward three steps" (not (debug_is_valid_move board (1, 0) (4, 0)));

  (* Invalid moves *)
  assert_bool "Pawn cannot move sideways" (not (debug_is_valid_move board (6, 0) (6, 1)));
  assert_bool "Pawn cannot move diagonally without capture" (not (debug_is_valid_move board (6, 0) (5, 1)));

  (* Capture *)
  let board_with_capture = Board.move_piece board (1, 1) (5, 1) in
  print_endline "Board State After Move (1,1) to (5,1):";
  print_board_debug board_with_capture;
  assert_bool "White pawn can capture diagonally" (debug_is_valid_move board_with_capture (6, 0) (5, 1))

(* Test algebraic conversions *)
let test_algebraic_conversion _ =
  assert_equal (0, 0) (Board.algebraic_to_indices "a8");
  assert_equal "a8" (Board.indices_to_algebraic (0, 0));
  assert_equal (7, 7) (Board.algebraic_to_indices "h1");
  assert_equal "h1" (Board.indices_to_algebraic (7, 7));
  assert_raises (Failure "Invalid position") (fun () -> Board.algebraic_to_indices "i1")

let suite =
  "Chess Tests" >::: [
    "test_init_board" >:: test_init_board;
    "test_valid_position" >:: test_valid_position;
    "test_valid_move" >:: test_valid_move;
    "test_algebraic_conversion" >:: test_algebraic_conversion;
  ]

let () = run_test_tt_main suite
