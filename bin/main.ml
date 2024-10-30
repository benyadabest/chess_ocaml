open Board
open Ui

let () =
  let board = init_board () in

  (* Predefine a list of moves *)
  let moves = [
    "e2 e4";  (* White moves pawn from e2 to e4 *)
    "e7 e5";  (* Black moves pawn from e7 to e5 *)
  ] in

  (* Simulate the moves *)
  game_loop board moves
