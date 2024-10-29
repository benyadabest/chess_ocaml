(* Define types *)
type piece_type = 
  | King 
  | Queen 
  | Rook 
  | Bishop 
  | Knight 
  | Pawn

type color = 
  | White 
  | Black

type piece = {
  piece_type : piece_type;
  color : color;
}

(* Helper function to generate moves along straight lines for Rook, Queen *)
let rec generate_straight_moves (row, col) direction board_size =
  match direction with
  | (dr, dc) ->
    let rec loop (r, c) acc =
      let new_r = r + dr in
      let new_c = c + dc in
      if new_r >= 0 && new_r < board_size && new_c >= 0 && new_c < board_size
      then loop (new_r, new_c) ((new_r, new_c) :: acc)
      else acc
    in
    loop (row, col) []

(* Generate moves for diagonal directions *)
let generate_diagonal_moves (row, col) board_size =
  let directions = [(-1, -1); (-1, 1); (1, -1); (1, 1)] in
  directions

(* Generate moves for rook-like directions (straight lines) *)
let generate_rook_moves (row, col) board_size =
  let directions = [(1, 0); (-1, 0); (0, 1); (0, -1)] in
  directions

(* Generate knight moves *)
let generate_knight_moves (row, col) =
  let knight_moves = [(2, 1); (2, -1); (-2, 1); (-2, -1); (1, 2); (1, -2); (-1, 2); (-1, -2)] in
  knight_moves

(* Legal move generator for each piece *)
let legal_moves piece (row, col) =
  match piece.piece_type with
  | King -> 
    let king_moves = [(-1, 0); (1, 0); (0, -1); (0, 1); (-1, -1); (-1, 1); (1, -1); (1, 1)] in
    List.filter (fun (r, c) -> r >= 0 && r < 8 && c >= 0 && c < 8) 
      (List.map (fun (dr, dc) -> (row + dr, col + dc)) king_moves)

  | Queen -> 
    List.append (generate_rook_moves (row, col) 8) (generate_diagonal_moves (row, col) 8)

  | Rook -> 
    generate_rook_moves (row, col) 8

  | Bishop -> 
    generate_diagonal_moves (row, col) 8

  | Knight -> 
    generate_knight_moves (row, col)

  | Pawn -> 
    if piece.color = White then 
      if row = 1 then [(row + 1, col); (row + 2, col)] 
      else [(row + 1, col)]
    else 
      if row = 6 then [(row - 1, col); (row - 2, col)]
      else [(row - 1, col)]