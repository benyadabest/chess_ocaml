open Pieces

type square = Pieces.piece option
type board = square array array

(* ANSI color codes for visualization *)
let red = "\027[31m"
let blue = "\027[34m"
let reset = "\027[0m"

(* Helper function to check valid board positions *)
let is_valid_position (row, col) =
  row >= 0 && row < 8 && col >= 0 && col < 8

(* Retrieve a piece from the board *)
let get_piece_at (board : board) (row, col) : square =
  if is_valid_position (row, col) then
    board.(row).(col)
  else
    failwith "Invalid position"

(* Locate the king on the board *)
let find_king (board : board) (king_color : string) : (int * int) =
  let rec find pos_list =
    match pos_list with
    | [] -> failwith "King not found"
    | (row, col) :: rest -> (
        match get_piece_at board (row, col) with
        | Some piece when get_piece_type piece = "King" && get_color piece = king_color -> (row, col)
        | _ -> find rest
      )
  in
  find (List.init 8 (fun row -> List.init 8 (fun col -> (row, col))) |> List.concat)

(* Validate a move using the pieces module's valid moves list *)
let is_valid_move (board : board) (start_row, start_col) (end_row, end_col) : bool =
  if not (is_valid_position (start_row, start_col)) || 
     not (is_valid_position (end_row, end_col)) then
    false
  else
    match get_piece_at board (start_row, start_col) with
    | None -> false
    | Some piece ->
        let valid_moves = valid_piece_move piece (start_row, start_col) in
        List.exists (fun (row, col) -> row = end_row && col = end_col) valid_moves &&
        (match get_piece_at board (end_row, end_col) with
         | None -> true
         | Some target_piece -> get_color target_piece <> get_color piece)

(* Move a piece on the board *)
let move_piece (board : board) (start_pos : int * int) (end_pos : int * int) : board =
  let start_row, start_col = start_pos in
  let end_row, end_col = end_pos in

  if not (is_valid_move board start_pos end_pos) then
    failwith "Invalid move"
  else
    match get_piece_at board start_pos with
    | None -> failwith "No piece at start position"
    | Some piece ->
        let new_board = Array.map Array.copy board in
        new_board.(start_row).(start_col) <- None;
        new_board.(end_row).(end_col) <- Some piece;
        new_board

(* Check if a move would put the king in check *)
let would_be_in_check (board : board) (start_pos : int * int) (end_pos : int * int) : bool =
  match get_piece_at board start_pos with
  | None -> false
  | Some piece ->
      let new_board = 
        let temp_board = Array.map Array.copy board in
        temp_board.(fst start_pos).(snd start_pos) <- None;
        temp_board.(fst end_pos).(snd end_pos) <- Some piece;
        temp_board
      in
      let king_color = get_color piece in
      let king_pos = find_king new_board king_color in
      List.exists (fun (row, col) ->
        match get_piece_at new_board (row, col) with
        | Some enemy_piece when get_color enemy_piece <> king_color ->
            List.exists ((=) king_pos) (valid_piece_move enemy_piece (row, col))
        | _ -> false
      ) (List.init 8 (fun row -> List.init 8 (fun col -> (row, col))) |> List.concat)

(* Check if the king of a given color is in check *)
let is_check (board : board) (king_color : string) : bool =
  let king_pos = find_king board king_color in
  List.exists (fun (row, col) ->
    match get_piece_at board (row, col) with
    | Some enemy_piece when get_color enemy_piece <> king_color ->
        List.exists ((=) king_pos) (valid_piece_move enemy_piece (row, col))
    | _ -> false
  ) (List.init 8 (fun row -> List.init 8 (fun col -> (row, col))) |> List.concat)

(* Check if the king of a given color is in checkmate *)
let is_checkmate (board : board) (king_color : string) : bool =
  if not (is_check board king_color) then false
  else
    let pieces =
      List.filter_map (fun (row, col) ->
        match get_piece_at board (row, col) with
        | Some piece when get_color piece = king_color -> Some (piece, (row, col))
        | _ -> None
      ) (List.init 8 (fun row -> List.init 8 (fun col -> (row, col))) |> List.concat)
    in
    not (List.exists (fun (piece, (start_row, start_col)) ->
      let valid_moves = valid_piece_move piece (start_row, start_col) in
      List.exists (fun (end_row, end_col) ->
        is_valid_move board (start_row, start_col) (end_row, end_col) &&
        not (would_be_in_check board (start_row, start_col) (end_row, end_col))
      ) valid_moves
    ) pieces)

(* Initialize the chessboard with pieces in their starting positions *)
let init_board () : board =
  let board = Array.make_matrix 8 8 None in

  let place_piece row col piece_type color =
    board.(row).(col) <- Some (create_piece piece_type color)
  in

  (* Place main pieces for both sides *)
  List.iter (fun (row, color) ->
    place_piece row 0 "Rook" color;
    place_piece row 1 "Knight" color;
    place_piece row 2 "Bishop" color;
    place_piece row 3 "Queen" color;
    place_piece row 4 "King" color;
    place_piece row 5 "Bishop" color;
    place_piece row 6 "Knight" color;
    place_piece row 7 "Rook" color
  ) [(0, "Black"); (7, "White")];

  (* Place pawns *)
  for col = 0 to 7 do
    place_piece 1 col "Pawn" "Black";
    place_piece 6 col "Pawn" "White";
  done;

  board

(* Print the chessboard *)
let print_board (board : board) : unit =
  print_endline "  +-----------------------+";
  for row = 0 to 7 do
    Printf.printf " %d | " (8 - row);
    for col = 0 to 7 do
      match board.(row).(col) with
      | None -> print_string "- "
      | Some piece ->
          let color = match get_color piece with
            | "White" -> red
            | "Black" -> blue
            | _ -> reset
          in
          let piece_str = match get_piece_type piece with
            | "King" -> "K"
            | "Queen" -> "Q"
            | "Rook" -> "R"
            | "Bishop" -> "B"
            | "Knight" -> "N"
            | "Pawn" -> "P"
            | _ -> "?"
          in
          Printf.printf "%s%s%s " color piece_str reset
    done;
    print_endline "|";
  done;
  print_endline "  +-----------------------+";
  print_endline "    a  b  c  d  e  f  g  h\n"

(* Convert algebraic notation to array indices *)
let algebraic_to_indices notation =
  if String.length notation <> 2 then
    failwith "Invalid position";
  let col = Char.code (Char.lowercase_ascii notation.[0]) - Char.code 'a' in
  let row = 8 - (int_of_char notation.[1] - int_of_char '0') in
  if not (is_valid_position (row, col)) then
    failwith "Invalid position";
  (row, col)

(* Convert array indices to algebraic notation *)
let indices_to_algebraic (row, col) =
  let col_char = char_of_int (col + Char.code 'a') in
  let row_char = char_of_int (8 - row + Char.code '0') in
  String.make 1 col_char ^ String.make 1 row_char

(* Move a piece using algebraic notation *)
let move_piece_algebraic (board : board) start_pos end_pos : board =
  let start_indices = algebraic_to_indices start_pos in
  let end_indices = algebraic_to_indices end_pos in
  move_piece board start_indices end_indices
