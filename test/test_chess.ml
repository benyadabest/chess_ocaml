open OUnit2
open Chess

(** Testing for pieces compilation unit. *)
let white_king = Chess.Pieces.create_piece "King" "White"

let black_king = Chess.Pieces.create_piece "King" "Black"
let white_queen = Chess.Pieces.create_piece "Queen" "White"
let black_queen = Chess.Pieces.create_piece "Queen" "Black"
let white_rook = Chess.Pieces.create_piece "Rook" "White"
let black_rook = Chess.Pieces.create_piece "Rook" "Black"
let white_bishop = Chess.Pieces.create_piece "Bishop" "White"
let black_bishop = Chess.Pieces.create_piece "Bishop" "Black"
let white_knight = Chess.Pieces.create_piece "Knight" "White"
let black_knight = Chess.Pieces.create_piece "Knight" "Black"
let white_pawn = Chess.Pieces.create_piece "Pawn" "White"
let black_pawn = Chess.Pieces.create_piece "Pawn" "Black"

(** [compare_pos_lists l1 l2] sorts and compares two lists. *)
let compare_pos_lists l1 l2 =
  let sort_pos_list = List.sort compare in
  assert_equal (sort_pos_list l1) (sort_pos_list l2)

let piece_creation_tests =
  [
    ( "test_create_white_king" >:: fun _ ->
      assert_equal "King" (Chess.Pieces.get_piece_type white_king);
      assert_equal "White" (Chess.Pieces.get_color white_king) );
    ( "test_create_black_pawn" >:: fun _ ->
      assert_equal "Pawn" (Chess.Pieces.get_piece_type black_pawn);
      assert_equal "Black" (Chess.Pieces.get_color black_pawn) );
    ( "test_invalid_piece_type" >:: fun _ ->
      assert_raises (Failure "Error: Unknown Piece.") (fun () ->
          Chess.Pieces.create_piece "Invalid" "White") );
    ( "test_invalid_color" >:: fun _ ->
      assert_raises (Failure "Error: Unknown Piece color.") (fun () ->
          Chess.Pieces.create_piece "King" "Invalid") );
  ]

let position_validation_tests =
  [
    ( "test_valid_positions" >:: fun _ ->
      assert_equal true (Chess.Pieces.valid_piece_pos (0, 0));
      assert_equal true (Chess.Pieces.valid_piece_pos (7, 7));
      assert_equal true (Chess.Pieces.valid_piece_pos (3, 4)) );
    ( "test_invalid_positions" >:: fun _ ->
      assert_equal false (Chess.Pieces.valid_piece_pos (-1, 0));
      assert_equal false (Chess.Pieces.valid_piece_pos (8, 0));
      assert_equal false (Chess.Pieces.valid_piece_pos (0, -1));
      assert_equal false (Chess.Pieces.valid_piece_pos (0, 8)) );
  ]

let pawn_movement_tests =
  [
    ( "test_white_pawn_center_moves" >:: fun _ ->
      let expected = [ (4, 3); (4, 2); (4, 4); (5, 3) ] in
      compare_pos_lists expected
        (Chess.Pieces.valid_pawn_move white_pawn (3, 3)) );
    ( "test_white_pawn_right_edge_moves" >:: fun _ ->
      let expected = [ (4, 7); (4, 6); (5, 7) ] in
      compare_pos_lists expected
        (Chess.Pieces.valid_pawn_move white_pawn (3, 7)) );
    ( "test_black_pawn_center_moves" >:: fun _ ->
      let expected = [ (2, 3); (2, 2); (2, 4); (1, 3) ] in
      compare_pos_lists expected
        (Chess.Pieces.valid_pawn_move black_pawn (3, 3)) );
    ( "test_black_pawn_right_edge_moves" >:: fun _ ->
      let expected = [ (2, 7); (2, 6); (1, 7) ] in
      compare_pos_lists expected
        (Chess.Pieces.valid_pawn_move black_pawn (3, 7)) );
    ( "test_black_pawn_left_edge_moves" >:: fun _ ->
      let expected = [ (2, 0); (2, 1); (1, 0) ] in
      compare_pos_lists expected
        (Chess.Pieces.valid_pawn_move black_pawn (3, 0)) );
    ( "test_white_pawn_left_edge_moves" >:: fun _ ->
      let expected = [ (4, 0); (4, 1); (5, 0) ] in
      compare_pos_lists expected
        (Chess.Pieces.valid_pawn_move white_pawn (3, 0)) );
    ( "test_white_pawn_last_rank" >:: fun _ ->
      compare_pos_lists [] (Chess.Pieces.valid_pawn_move white_pawn (7, 3)) );
    ( "test_black_pawn_first_rank" >:: fun _ ->
      compare_pos_lists [] (Chess.Pieces.valid_pawn_move black_pawn (0, 3)) );
  ]

let rook_movement_tests =
  [
    ( "test_rook_center_moves" >:: fun _ ->
      let expected =
        [ (2, 3); (1, 3); (0, 3); (4, 3); (5, 3); (6, 3); (7, 3) ]
        @ [ (3, 2); (3, 1); (3, 0); (3, 4); (3, 5); (3, 6); (3, 7) ]
      in

      compare_pos_lists expected (Chess.Pieces.valid_rook_move (3, 3)) );
    ( "test_rook_corner_moves" >:: fun _ ->
      let expected =
        [ (1, 0); (2, 0); (3, 0); (4, 0); (5, 0); (6, 0); (7, 0) ]
        @ [ (0, 1); (0, 2); (0, 3); (0, 4); (0, 5); (0, 6); (0, 7) ]
      in

      compare_pos_lists expected (Chess.Pieces.valid_rook_move (0, 0)) );
    ( "test_rook_corner_moves_top_right" >:: fun _ ->
      let expected =
        [ (0, 0); (0, 1); (0, 2); (0, 3); (0, 4); (0, 5); (0, 6) ]
        @ [ (1, 7); (2, 7); (3, 7); (4, 7); (5, 7); (6, 7); (7, 7) ]
      in
      compare_pos_lists expected (Chess.Pieces.valid_rook_move (0, 7)) );
    ( "test_rook_corner_moves_bottom_left" >:: fun _ ->
      let expected =
        [ (0, 0); (1, 0); (2, 0); (3, 0); (4, 0); (5, 0); (6, 0) ]
        @ [ (7, 1); (7, 2); (7, 3); (7, 4); (7, 5); (7, 6); (7, 7) ]
      in
      compare_pos_lists expected (Chess.Pieces.valid_rook_move (7, 0)) );
  ]

let knight_movement_tests =
  [
    ( "test_knight_center_moves" >:: fun _ ->
      let expected =
        [ (5, 4); (4, 5); (2, 5); (1, 4); (1, 2); (2, 1); (4, 1); (5, 2) ]
      in
      compare_pos_lists expected (Chess.Pieces.valid_knight_move (3, 3)) );
    ( "test_knight_corner_moves" >:: fun _ ->
      let expected = [ (2, 1); (1, 2) ] in
      compare_pos_lists expected (Chess.Pieces.valid_knight_move (0, 0)) );
    ( "test_knight_corner_moves_top_right" >:: fun _ ->
      let expected = [ (2, 6); (1, 5) ] in
      compare_pos_lists expected (Chess.Pieces.valid_knight_move (0, 7)) );
    ( "test_knight_corner_moves_bottom_right" >:: fun _ ->
      let expected = [ (5, 6); (6, 5) ] in
      compare_pos_lists expected (Chess.Pieces.valid_knight_move (7, 7)) );
    ( "test_knight_corner_moves_bottom_left" >:: fun _ ->
      let expected = [ (5, 1); (6, 2) ] in
      compare_pos_lists expected (Chess.Pieces.valid_knight_move (7, 0)) );
    ( "test_knight_edge_moves" >:: fun _ ->
      let expected = [ (2, 1); (3, 2); (5, 2); (6, 1) ] in
      compare_pos_lists expected (Chess.Pieces.valid_knight_move (4, 0)) );
  ]

let bishop_movement_tests =
  [
    ( "test_bishop_center_moves" >:: fun _ ->
      let expected =
        [ (4, 4); (5, 5); (6, 6); (7, 7) ]
        @ [ (4, 2); (5, 1); (6, 0) ]
        @ [ (2, 4); (1, 5); (0, 6) ]
        @ [ (2, 2); (1, 1); (0, 0) ]
      in

      compare_pos_lists expected (Chess.Pieces.valid_bishop_move (3, 3)) );
    ( "test_bishop_corner_moves" >:: fun _ ->
      let expected =
        [ (1, 1); (2, 2); (3, 3); (4, 4); (5, 5); (6, 6); (7, 7) ]
      in
      compare_pos_lists expected (Chess.Pieces.valid_bishop_move (0, 0)) );
    ( "test_bishop_corner_moves_top_right" >:: fun _ ->
      let expected =
        [ (1, 6); (2, 5); (3, 4); (4, 3); (5, 2); (6, 1); (7, 0) ]
      in
      compare_pos_lists expected (Chess.Pieces.valid_bishop_move (0, 7)) );
    ( "test_bishop_corner_moves_bottom_right" >:: fun _ ->
      let expected =
        [ (6, 6); (5, 5); (4, 4); (3, 3); (2, 2); (1, 1); (0, 0) ]
      in
      compare_pos_lists expected (Chess.Pieces.valid_bishop_move (7, 7)) );
  ]

let queen_movement_tests =
  [
    ( "test_queen_center_moves" >:: fun _ ->
      let expected =
        [ (2, 3); (1, 3); (0, 3); (4, 3); (5, 3); (6, 3); (7, 3) ]
        @ [ (3, 2); (3, 1); (3, 0); (3, 4); (3, 5); (3, 6); (3, 7) ]
        @ [ (4, 4); (5, 5); (6, 6); (7, 7) ]
        @ [ (4, 2); (5, 1); (6, 0) ]
        @ [ (2, 4); (1, 5); (0, 6) ]
        @ [ (2, 2); (1, 1); (0, 0) ]
      in
      compare_pos_lists expected (Chess.Pieces.valid_queen_move (3, 3)) );
    ( "test_queen_corner_moves" >:: fun _ ->
      let expected =
        (* Rook-like moves *)
        [ (1, 0); (2, 0); (3, 0); (4, 0); (5, 0); (6, 0); (7, 0) ]
        @ [ (0, 1); (0, 2); (0, 3); (0, 4); (0, 5); (0, 6); (0, 7) ]
        @ (* Bishop-like moves *)
        [ (1, 1); (2, 2); (3, 3); (4, 4); (5, 5); (6, 6); (7, 7) ]
      in
      compare_pos_lists expected (Chess.Pieces.valid_queen_move (0, 0)) );
    ( "test_queen_edge_moves" >:: fun _ ->
      let expected =
        (* Rook-like moves *)
        [ (0, 0); (1, 0); (2, 0); (3, 0); (4, 0); (5, 0); (6, 0) ]
        @ [ (7, 1); (7, 2); (7, 3); (7, 4); (7, 5); (7, 6); (7, 7) ]
        @ (* Bishop-like moves *)
        [ (6, 1); (5, 2); (4, 3); (3, 4); (2, 5); (1, 6); (0, 7) ]
      in
      compare_pos_lists expected (Chess.Pieces.valid_queen_move (7, 0)) );
  ]

let king_movement_tests =
  [
    ( "test_king_center_moves" >:: fun _ ->
      let expected =
        [ (4, 3); (4, 4); (3, 4); (2, 4); (2, 3); (2, 2); (3, 2); (4, 2) ]
      in
      compare_pos_lists expected (Chess.Pieces.valid_king_move (3, 3)) );
    ( "test_king_corner_moves" >:: fun _ ->
      let expected = [ (1, 0); (1, 1); (0, 1) ] in
      compare_pos_lists expected (Chess.Pieces.valid_king_move (0, 0)) );
    ( "test_king_corner_moves_top_right" >:: fun _ ->
      let expected = [ (1, 7); (1, 6); (0, 6) ] in
      compare_pos_lists expected (Chess.Pieces.valid_king_move (0, 7)) );
    ( "test_king_corner_moves_bottom_right" >:: fun _ ->
      let expected = [ (6, 7); (6, 6); (7, 6) ] in
      compare_pos_lists expected (Chess.Pieces.valid_king_move (7, 7)) );
    ( "test_king_corner_moves_bottom_left" >:: fun _ ->
      let expected = [ (6, 0); (6, 1); (7, 1) ] in
      compare_pos_lists expected (Chess.Pieces.valid_king_move (7, 0)) );
    (* Test edge position *)
    ( "test_king_edge_moves" >:: fun _ ->
      let expected = [ (4, 0); (4, 1); (3, 1); (2, 1); (2, 0) ] in
      compare_pos_lists expected (Chess.Pieces.valid_king_move (3, 0)) );
  ]

let piece_string_tests =
  [
    ( "test_piece_to_string_white" >:: fun _ ->
      assert_equal "K" (Chess.Pieces.piece_to_string (Some white_king));
      assert_equal "Q" (Chess.Pieces.piece_to_string (Some white_queen));
      assert_equal "R" (Chess.Pieces.piece_to_string (Some white_rook));
      assert_equal "B" (Chess.Pieces.piece_to_string (Some white_bishop));
      assert_equal "N" (Chess.Pieces.piece_to_string (Some white_knight));
      assert_equal "P" (Chess.Pieces.piece_to_string (Some white_pawn)) );
    ( "test_piece_to_string_black" >:: fun _ ->
      assert_equal "K" (Chess.Pieces.piece_to_string (Some black_king));
      assert_equal "Q" (Chess.Pieces.piece_to_string (Some black_queen));
      assert_equal "R" (Chess.Pieces.piece_to_string (Some black_rook));
      assert_equal "B" (Chess.Pieces.piece_to_string (Some black_bishop));
      assert_equal "N" (Chess.Pieces.piece_to_string (Some black_knight));
      assert_equal "P" (Chess.Pieces.piece_to_string (Some black_pawn)) );
    ( "test_piece_to_string_none" >:: fun _ ->
      assert_equal "" (Chess.Pieces.piece_to_string None) );
  ]

let invalid_position_tests =
  [
    (* Test invalid positions for each piece move function *)
    ( "test_invalid_piece_moves" >:: fun _ ->
      List.iter
        (fun pos ->
          assert_raises (Failure "Invalid piece position.") (fun () ->
              Chess.Pieces.valid_piece_move white_pawn pos);
          assert_raises (Failure "Invalid piece position.") (fun () ->
              Chess.Pieces.valid_piece_move white_rook pos);
          assert_raises (Failure "Invalid piece position.") (fun () ->
              Chess.Pieces.valid_piece_move white_knight pos);
          assert_raises (Failure "Invalid piece position.") (fun () ->
              Chess.Pieces.valid_piece_move white_bishop pos);
          assert_raises (Failure "Invalid piece position.") (fun () ->
              Chess.Pieces.valid_piece_move white_queen pos);
          assert_raises (Failure "Invalid piece position.") (fun () ->
              Chess.Pieces.valid_piece_move white_king pos))
        [ (-1, 0); (8, 0); (0, -1); (0, 8); (8, 8); (-1, -1) ] );
  ]

(** End testing for pieces compilation unit. *)

(* Equality function for pieces *)

let pieces_equal p1 p2 =
  match (p1, p2) with
  | None, None -> true
  | Some piece1, Some piece2 ->
      Pieces.get_piece_type piece1 = Pieces.get_piece_type piece2
      && Pieces.get_color piece1 = Pieces.get_color piece2
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
  assert_bool "Black Rook at (0, 0)"
    (pieces_equal
       (Board.get_piece_at board (0, 0))
       (Some (Pieces.create_piece "Rook" "Black")));
  assert_bool "Black Knight at (0, 1)"
    (pieces_equal
       (Board.get_piece_at board (0, 1))
       (Some (Pieces.create_piece "Knight" "Black")));
  assert_bool "Black King at (0, 4)"
    (pieces_equal
       (Board.get_piece_at board (0, 4))
       (Some (Pieces.create_piece "King" "Black")));
  assert_bool "White Pawn at (6, 0)"
    (pieces_equal
       (Board.get_piece_at board (6, 0))
       (Some (Pieces.create_piece "Pawn" "White")));
  assert_bool "White King at (7, 4)"
    (pieces_equal
       (Board.get_piece_at board (7, 4))
       (Some (Pieces.create_piece "King" "White")));
  (* Ensure empty squares are indeed empty *)
  assert_bool "Empty square at (4, 4)"
    (pieces_equal (Board.get_piece_at board (4, 4)) None);
  assert_bool "Empty square at (5, 5)"
    (pieces_equal (Board.get_piece_at board (5, 5)) None)

(* Test valid position checking *)
let test_valid_position _ =
  assert_bool "Position (0, 0) should be valid" (Board.is_valid_position (0, 0));
  assert_bool "Position (7, 7) should be valid" (Board.is_valid_position (7, 7));
  assert_bool "Position (-1, 0) should be invalid"
    (not (Board.is_valid_position (-1, 0)));
  assert_bool "Position (8, 0) should be invalid"
    (not (Board.is_valid_position (8, 0)))

(* Test move validity *)
let test_valid_move _ =
  let board = Board.init_board () in
  print_endline "Initial Board State:";
  print_board_debug board;

  (* White Pawn movement *)
  assert_bool "White pawn can move forward one step"
    (debug_is_valid_move board (6, 0) (5, 0));
  assert_bool "White pawn can move forward two steps initially"
    (debug_is_valid_move board (6, 0) (4, 0));
  assert_bool "White pawn cannot move forward three steps"
    (not (debug_is_valid_move board (6, 0) (3, 0)));

  (* Black Pawn movement *)
  assert_bool "Black pawn can move forward one step"
    (debug_is_valid_move board (1, 0) (2, 0));
  assert_bool "Black pawn can move forward two steps initially"
    (debug_is_valid_move board (1, 0) (3, 0));
  assert_bool "Black pawn cannot move forward three steps"
    (not (debug_is_valid_move board (1, 0) (4, 0)));

  (* Invalid moves *)
  assert_bool "Pawn cannot move sideways"
    (not (debug_is_valid_move board (6, 0) (6, 1)));
  assert_bool "Pawn cannot move diagonally without capture"
    (not (debug_is_valid_move board (6, 0) (5, 1)));

  (* Capture *)
  let board_with_capture = Board.move_piece board (1, 1) (5, 1) in
  print_endline "Board State After Move (1,1) to (5,1):";
  print_board_debug board_with_capture;
  assert_bool "White pawn can capture diagonally"
    (debug_is_valid_move board_with_capture (6, 0) (5, 1))

(* Test algebraic conversions *)
let test_algebraic_conversion _ =
  assert_equal (0, 0) (Board.algebraic_to_indices "a8");
  assert_equal "a8" (Board.indices_to_algebraic (0, 0));
  assert_equal (7, 7) (Board.algebraic_to_indices "h1");
  assert_equal "h1" (Board.indices_to_algebraic (7, 7));
  assert_raises (Failure "Invalid position") (fun () ->
      Board.algebraic_to_indices "i1")

let suite =
  "Chess Tests"
  >::: [
         "test_init_board" >:: test_init_board;
         "test_valid_position" >:: test_valid_position;
         "test_algebraic_conversion" >:: test_algebraic_conversion;
       ]
       @ List.flatten
           [
             piece_creation_tests;
             position_validation_tests;
             pawn_movement_tests;
             rook_movement_tests;
             knight_movement_tests;
             bishop_movement_tests;
             queen_movement_tests;
             king_movement_tests;
             piece_string_tests;
             invalid_position_tests;
           ]

let () = run_test_tt_main suite
