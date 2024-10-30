open Pieces
open Board

(* Helper function to print a single square *)
let print_square (sq : square) =
  match sq with
  | None -> print_string ". "
  | Some {piece_type; color} ->
      let piece_char = match piece_type with
        | King -> "K"
        | Queen -> "Q"
        | Rook -> "R"
        | Bishop -> "B"
        | Knight -> "N"
        | Pawn -> "P"
      in
      let colored_piece = if color = White then piece_char else String.lowercase_ascii piece_char in
      print_string (colored_piece ^ " ")

(* Display the chessboard *)
let display_board (board : board) =
  for row = 7 downto 0 do
    print_int (row + 1); (* Print row numbers *)
    print_string " ";
    for col = 0 to 7 do
      print_square board.(row).(col)
    done;
    print_newline ()
  done;
  print_string "  a b c d e f g h\n"; (* Print column labels *)

(* Convert algebraic notation (like "e2") to board coordinates (row, col) *)
let parse_position (pos : string) : (int * int) option =
  if String.length pos <> 2 then None
  else
    let col = Char.code pos.[0] - Char.code 'a' in
    let row = Char.code pos.[1] - Char.code '1' in
    if row >= 0 && row <= 7 && col >= 0 && col <= 7 then Some (row, col)
    else None

(* Parse a move in the format "e2 e4" *)
let parse_move (move : string) : ((int * int) * (int * int)) option =
  let parts = String.split_on_char ' ' move in
  match parts with
  | [start_pos; end_pos] ->
      (match parse_position start_pos, parse_position end_pos with
      | Some start_coords, Some end_coords -> Some (start_coords, end_coords)
      | _ -> None)
  | _ -> None

(* Simulate a sequence of moves and update the game state *)
let game_loop (board : board) (moves : string list) =
  let rec loop board moves =
    match moves with
    | [] -> 
        print_endline "No more moves.";
        ()
    | move :: rest ->
        (match parse_move move with
        | Some ((start_row, start_col), (end_row, end_col)) ->
            let piece = get_piece_at board (start_row, start_col) in
            (match piece with
            | None -> 
                print_endline ("No piece at " ^ move ^ ". Skipping.");
                loop board rest
            | Some p ->
                print_endline ("Move: " ^ move);
                let new_board = move_piece board (start_row, start_col) (end_row, end_col) in
                display_board new_board;
                loop new_board rest)
        | None -> 
            print_endline ("Invalid move format: " ^ move ^ ". Skipping.");
            loop board rest)
  
