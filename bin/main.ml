open Chess

(* Print the initial board state *)
let print_initial_board () =
  let board = Board.init_board () in
  Board.print_board board

(* Move every piece at least once *)
let move_every_piece () =
  let board = ref (Board.init_board ()) in

  (* Define a series of valid moves for each piece type *)
  let moves = [
    ((6, 0), (4, 0)); (* White Pawn *)
    ((1, 0), (3, 0)); (* Black Pawn *)
    ((7, 1), (5, 2)); (* White Knight *)
    ((0, 1), (2, 2)); (* Black Knight *)
    ((7, 2), (5, 0)); (* White Bishop *)
    ((0, 2), (2, 0)); (* Black Bishop *)
    ((7, 0), (5, 0)); (* White Rook *)
    ((0, 0), (2, 0)); (* Black Rook *)
    ((7, 3), (5, 1)); (* White Queen *)
    ((0, 3), (2, 1)); (* Black Queen *)
    ((7, 4), (6, 4)); (* White King *)
    ((0, 4), (1, 4)); (* Black King *)
  ] in

  (* Execute the moves and print the board after each move *)
  List.iter (fun (start_pos, end_pos) ->
    Printf.printf "Moving piece from %s to %s\n"
      (Board.indices_to_algebraic start_pos)
      (Board.indices_to_algebraic end_pos);
    board := Board.move_piece !board start_pos end_pos;
    Board.print_board !board;
    print_endline ""
  ) moves

let () =
  print_endline "Initial Board State:";
  print_initial_board ();
  print_endline "\nPerforming moves:";
  move_every_piece ()
