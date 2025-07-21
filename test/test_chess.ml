open OUnit2
open Chess
open Chess.Board
open Chess.Pieces

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
  assert_equal
    ~printer:(fun lst ->
      "["
      ^ String.concat "; "
          (List.map (fun (r, c) -> Printf.sprintf "(%d,%d)" r c) lst)
      ^ "]")
    (sort_pos_list l1) (sort_pos_list l2)

let piece_creation_tests =
  [
    ( "test_create_white_king" >:: fun _ ->
      assert_equal
        ~printer:(fun x -> x)
        "King"
        (Chess.Pieces.get_piece_type white_king);
      assert_equal
        ~printer:(fun x -> x)
        "White"
        (Chess.Pieces.get_color white_king) );
    ( "test_create_black_pawn" >:: fun _ ->
      assert_equal
        ~printer:(fun x -> x)
        "Pawn"
        (Chess.Pieces.get_piece_type black_pawn);
      assert_equal
        ~printer:(fun x -> x)
        "Black"
        (Chess.Pieces.get_color black_pawn) );
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
      assert_equal ~printer:string_of_bool true
        (Chess.Pieces.valid_piece_pos (0, 0));
      assert_equal ~printer:string_of_bool true
        (Chess.Pieces.valid_piece_pos (7, 7));
      assert_equal ~printer:string_of_bool true
        (Chess.Pieces.valid_piece_pos (3, 4)) );
    ( "test_invalid_positions" >:: fun _ ->
      assert_equal ~printer:string_of_bool false
        (Chess.Pieces.valid_piece_pos (-1, 0));
      assert_equal ~printer:string_of_bool false
        (Chess.Pieces.valid_piece_pos (8, 0));
      assert_equal ~printer:string_of_bool false
        (Chess.Pieces.valid_piece_pos (0, -1));
      assert_equal ~printer:string_of_bool false
        (Chess.Pieces.valid_piece_pos (0, 8)) );
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
        [ (1, 0); (2, 0); (3, 0); (4, 0); (5, 0); (6, 0); (7, 0) ]
        @ [ (0, 1); (0, 2); (0, 3); (0, 4); (0, 5); (0, 6); (0, 7) ]
        @ [ (1, 1); (2, 2); (3, 3); (4, 4); (5, 5); (6, 6); (7, 7) ]
      in
      compare_pos_lists expected (Chess.Pieces.valid_queen_move (0, 0)) );
    ( "test_queen_edge_moves" >:: fun _ ->
      let expected =
        [ (0, 0); (1, 0); (2, 0); (3, 0); (4, 0); (5, 0); (6, 0) ]
        @ [ (7, 1); (7, 2); (7, 3); (7, 4); (7, 5); (7, 6); (7, 7) ]
        @ [ (6, 1); (5, 2); (4, 3); (3, 4); (2, 5); (1, 6); (0, 7) ]
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
    ( "test_king_edge_moves" >:: fun _ ->
      let expected = [ (4, 0); (4, 1); (3, 1); (2, 1); (2, 0) ] in
      compare_pos_lists expected (Chess.Pieces.valid_king_move (3, 0)) );
  ]

let piece_string_tests =
  [
    ( "test_piece_to_string_white" >:: fun _ ->
      assert_equal
        ~printer:(fun x -> x)
        "K"
        (Chess.Pieces.piece_to_string (Some white_king));
      assert_equal
        ~printer:(fun x -> x)
        "Q"
        (Chess.Pieces.piece_to_string (Some white_queen));
      assert_equal
        ~printer:(fun x -> x)
        "R"
        (Chess.Pieces.piece_to_string (Some white_rook));
      assert_equal
        ~printer:(fun x -> x)
        "B"
        (Chess.Pieces.piece_to_string (Some white_bishop));
      assert_equal
        ~printer:(fun x -> x)
        "N"
        (Chess.Pieces.piece_to_string (Some white_knight));
      assert_equal
        ~printer:(fun x -> x)
        "P"
        (Chess.Pieces.piece_to_string (Some white_pawn)) );
    ( "test_piece_to_string_black" >:: fun _ ->
      assert_equal
        ~printer:(fun x -> x)
        "K"
        (Chess.Pieces.piece_to_string (Some black_king));
      assert_equal
        ~printer:(fun x -> x)
        "Q"
        (Chess.Pieces.piece_to_string (Some black_queen));
      assert_equal
        ~printer:(fun x -> x)
        "R"
        (Chess.Pieces.piece_to_string (Some black_rook));
      assert_equal
        ~printer:(fun x -> x)
        "B"
        (Chess.Pieces.piece_to_string (Some black_bishop));
      assert_equal
        ~printer:(fun x -> x)
        "N"
        (Chess.Pieces.piece_to_string (Some black_knight));
      assert_equal
        ~printer:(fun x -> x)
        "P"
        (Chess.Pieces.piece_to_string (Some black_pawn)) );
    ( "test_piece_to_string_none" >:: fun _ ->
      assert_equal ~printer:(fun x -> x) "" (Chess.Pieces.piece_to_string None)
    );
  ]

(** End testing for pieces compilation unit. *)

(** Testing for board compilation unit. *)

let pieces_equal p1 p2 =
  match (p1, p2) with
  | None, None -> true
  | Some p1, Some p2 ->
      Pieces.get_piece_type p1 = Pieces.get_piece_type p2
      && Pieces.get_color p1 = Pieces.get_color p2
  | _ -> false

let test_init_board _ =
  let board = Board.init_board () in
  List.iter
    (fun (row, col, piece_type, color) ->
      let expected_piece = Some (Pieces.create_piece piece_type color) in
      assert_bool
        (Printf.sprintf "%s %s at (%d, %d)" color piece_type row col)
        (pieces_equal (Board.get_piece_at board (row, col)) expected_piece))
    [
      (0, 0, "Rook", "Black");
      (0, 1, "Knight", "Black");
      (0, 2, "Bishop", "Black");
      (0, 3, "Queen", "Black");
      (0, 4, "King", "Black");
      (0, 5, "Bishop", "Black");
      (0, 6, "Knight", "Black");
      (0, 7, "Rook", "Black");
      (7, 0, "Rook", "White");
      (7, 1, "Knight", "White");
      (7, 2, "Bishop", "White");
      (7, 3, "Queen", "White");
      (7, 4, "King", "White");
      (7, 5, "Bishop", "White");
      (7, 6, "Knight", "White");
      (7, 7, "Rook", "White");
    ];
  for col = 0 to 7 do
    assert_bool
      (Printf.sprintf "Black Pawn at (1, %d)" col)
      (pieces_equal
         (Board.get_piece_at board (1, col))
         (Some (Pieces.create_piece "Pawn" "Black")));
    assert_bool
      (Printf.sprintf "White Pawn at (6, %d)" col)
      (pieces_equal
         (Board.get_piece_at board (6, col))
         (Some (Pieces.create_piece "Pawn" "White")))
  done;
  List.iter
    (fun (row, col) ->
      assert_bool
        (Printf.sprintf "Empty square at (%d, %d)" row col)
        (pieces_equal (Board.get_piece_at board (row, col)) None))
    [ (4, 4); (5, 5); (3, 3); (2, 0); (2, 7); (5, 0); (5, 7) ]

let test_get_piece_at _ =
  let board = Board.init_board () in
  List.iter
    (fun (row, col, expected) ->
      assert_bool
        (Printf.sprintf "Position (%d, %d)" row col)
        (pieces_equal (Board.get_piece_at board (row, col)) expected))
    [
      (0, 0, Some (Pieces.create_piece "Rook" "Black"));
      (7, 4, Some (Pieces.create_piece "King" "White"));
      (6, 0, Some (Pieces.create_piece "Pawn" "White"));
      (3, 3, None);
      (-1, 0, None);
      (8, 0, None);
      (0, 8, None);
      (0, -1, None);
    ]

let test_move_piece _ =
  let board = Board.init_board () in
  List.iter
    (fun (start_pos, end_pos, should_succeed) ->
      match should_succeed with
      | true ->
          let new_board = Board.move_piece board start_pos end_pos in
          let piece = Board.get_piece_at board start_pos in
          assert_bool "Start position is empty"
            (pieces_equal (Board.get_piece_at new_board start_pos) None);
          assert_bool "End position has moved piece"
            (pieces_equal (Board.get_piece_at new_board end_pos) piece)
      | false ->
          assert_raises (Failure "Invalid move") (fun () ->
              Board.move_piece board start_pos end_pos))
    [
      ((6, 4), (4, 4), true);
      ((7, 1), (5, 2), true);
      ((6, 0), (5, 0), true);
      ((7, 0), (6, 0), false);
      ((0, 0), (0, 2), false);
      ((3, 3), (4, 4), false);
      ((8, 8), (7, 7), false);
    ]

let test_valid_position _ =
  List.iter
    (fun (pos, expected) ->
      assert_equal ~printer:string_of_bool expected
        (Board.is_valid_position pos)
        ~msg:(Printf.sprintf "Position (%d, %d)" (fst pos) (snd pos)))
    [
      ((0, 0), true);
      ((7, 7), true);
      ((3, 4), true);
      ((0, 7), true);
      ((-1, 0), false);
      ((8, 0), false);
      ((0, -1), false);
      ((0, 8), false);
      ((8, 8), false);
      ((-1, -1), false);
    ]

(* A helper printer for (int * int) tuples *)
let print_pos (r, c) = Printf.sprintf "(%d, %d)" r c

let test_algebraic_conversion _ =
  List.iter
    (fun (alg, indices) ->
      assert_equal
        ~printer:(fun (r, c) -> Printf.sprintf "(%d,%d)" r c)
        indices
        (Board.algebraic_to_indices alg)
        ~msg:(Printf.sprintf "Converting %s" alg);
      assert_equal
        ~printer:(fun s -> s)
        alg
        (Board.indices_to_algebraic indices)
        ~msg:(Printf.sprintf "Converting (%d, %d)" (fst indices) (snd indices)))
    [
      ("a1", (7, 0));
      ("h1", (7, 7));
      ("a8", (0, 0));
      ("h8", (0, 7));
      ("e4", (4, 4));
      ("d6", (2, 3));
    ];
  assert_raises (Failure "Invalid notation format") (fun () ->
      Board.algebraic_to_indices "");
  assert_raises (Failure "Invalid notation format") (fun () ->
      Board.algebraic_to_indices "a");
  assert_raises (Failure "Invalid notation format") (fun () ->
      Board.algebraic_to_indices "1");
  assert_raises (Failure "Invalid notation format") (fun () ->
      Board.algebraic_to_indices "a11");
  assert_raises (Failure "Invalid position") (fun () ->
      Board.algebraic_to_indices "i1");
  assert_raises (Failure "Invalid position") (fun () ->
      Board.algebraic_to_indices "a9");
  assert_raises (Failure "Invalid position") (fun () ->
      Board.algebraic_to_indices "a0")

let test_move_piece_algebraic _ =
  let board = Board.init_board () in
  List.iter
    (fun (start_pos, end_pos, should_succeed) ->
      match should_succeed with
      | true ->
          let new_board = Board.move_piece_algebraic board start_pos end_pos in
          let start_indices = Board.algebraic_to_indices start_pos in
          let end_indices = Board.algebraic_to_indices end_pos in
          assert_bool "Piece moved successfully"
            (pieces_equal
               (Board.get_piece_at board start_indices)
               (Board.get_piece_at new_board end_indices))
      | false ->
          assert_raises (Failure "Invalid move") (fun () ->
              Board.move_piece_algebraic board start_pos end_pos))
    [
      ("e2", "e4", true);
      ("b1", "c3", true);
      ("a2", "a3", true);
      ("e1", "e3", false);
      ("h1", "h3", false);
    ]

let test_check _ =
  let board = Board.init_board () in
  assert_equal ~printer:string_of_bool false (Board.is_check board "White");
  assert_equal ~printer:string_of_bool false (Board.is_check board "Black");
  let board = Board.move_piece_algebraic board "f2" "f3" in
  let board = Board.move_piece_algebraic board "e7" "e6" in
  let board = Board.move_piece_algebraic board "g2" "g4" in
  let board = Board.move_piece_algebraic board "d8" "h4" in
  assert_equal ~printer:string_of_bool true (Board.is_check board "White")

let test_checkmate _ =
  let board = Board.init_board () in
  assert_equal ~printer:string_of_bool false (Board.is_checkmate board "White");
  assert_equal ~printer:string_of_bool false (Board.is_checkmate board "Black");
  let board = Board.move_piece_algebraic board "f2" "f3" in
  let board = Board.move_piece_algebraic board "e7" "e6" in
  let board = Board.move_piece_algebraic board "g2" "g4" in
  let board = Board.move_piece_algebraic board "d8" "h4" in
  assert_equal ~printer:string_of_bool true (Board.is_checkmate board "White")

let test_stalemate _ =
  let board = Board.init_board () in
  Array.iter (fun row -> Array.fill row 0 8 None) board;
  Board.place_piece board "a8" (Pieces.create_piece "King" "Black");
  Board.place_piece board "b6" (Pieces.create_piece "Queen" "White");
  Board.place_piece board "c7" (Pieces.create_piece "King" "White");
  assert_equal ~printer:string_of_bool true (Board.is_stalemate board "Black");
  assert_equal ~printer:string_of_bool false (Board.is_checkmate board "Black");
  assert_equal ~printer:string_of_bool false (Board.is_stalemate board "White")

let test_place_piece _ =
  let board = Board.init_board () in
  let piece = Pieces.create_piece "Queen" "White" in
  Board.place_piece board "e4" piece;
  assert_bool "Piece placed correctly"
    (pieces_equal (Board.get_piece_at board (4, 4)) (Some piece));
  assert_raises (Failure "Invalid position") (fun () ->
      Board.place_piece board "i1" piece);
  assert_raises (Failure "Invalid position") (fun () ->
      Board.place_piece board "a9" piece)

(** End testing for board compilation unit. *)

let suite =
  "Chess Tests"
  >::: [
         "test_init_board" >:: test_init_board;
         "test_get_piece_at" >:: test_get_piece_at;
         "test_move_piece" >:: test_move_piece;
         "test_valid_position" >:: test_valid_position;
         "test_algebraic_conversion" >:: test_algebraic_conversion;
         "test_move_piece_algebraic" >:: test_move_piece_algebraic;
         "test_check" >:: test_check;
         "test_checkmate" >:: test_checkmate;
         "test_stalemate" >:: test_stalemate;
         "test_place_piece" >:: test_place_piece;
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
           ]

let () = run_test_tt_main suite
