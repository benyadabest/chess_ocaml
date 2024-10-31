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

let game_over game_state = game_state.game_over
let is_whites_move game_state = game_state.is_whites_move

let print_board (game_state : game_state) : unit =
  let board_str = ref "" in
  for row = 7 downto 0 do
    board_str := !board_str ^ string_of_int row ^ "| ";
    for col = 0 to 7 do
      let piece_value =
        piece_to_int (get_piece_at game_state.board (row, col))
      in
      board_str := !board_str ^ Printf.sprintf "%3d" piece_value
    done;
    board_str := !board_str ^ "\n"
  done;
  board_str := !board_str ^ "   -------------------------\n";
  board_str := !board_str ^ "     0  1  2  3  4  5  6  7\n";
  print_endline !board_str

let move (game_state : game_state) (start_row, start_col) (end_row, end_col) :
    game_state =
  let piece_opt = get_piece_at game_state.board (start_row, start_col) in
  match piece_opt with
  | Some piece -> (
      match get_piece_type piece with
      | "Pawn" ->
          let new_board =
            move_piece game_state.board (start_row, start_col) (end_row, end_col)
          in
          { game_state with board = new_board }
      | _ -> failwith "Error: Only pawns can be moved so far.")
  | None -> failwith "Error: No piece at the starting position."
