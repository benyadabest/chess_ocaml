open Pieces
open Board

type game_state = {
  board : board;
  current_player : color;
  in_check : bool;
  game_over : bool;
}

let init_game () : game_state =
  {
    board = init_board ();
    current_player = White;
    in_check = false;
    game_over = false;
  }