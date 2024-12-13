(* AF: - A "piece_type" value (King, Queen, Rook, Bishop, Knight, or Pawn)
   represents the specific rank of the chess piece. A "color" value (White (Red)
   or Black (Blue)) represents which side the piece belongs to. A "piece" record
   combines these two to represent a single chess piece. For example, {
   piece_type = Pawn; color = White } represents a white pawn.

   RI: "piece_type" must be one of the six valid constructors (King, Queen,
   Rook, Bishop, Knight, or Pawn). "color" must be either White or Black. No
   additional constraints since any combination of [piece_type] and [color]
   forms a valid piece. *)
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

let create_piece (p : string) (c : string) =
  let p_type =
    match p with
    | "King" -> King
    | "Queen" -> Queen
    | "Rook" -> Rook
    | "Bishop" -> Bishop
    | "Knight" -> Knight
    | "Pawn" -> Pawn
    | _ -> failwith "Error: Unknown Piece."
  in
  let col =
    match c with
    | "White" -> White
    | "Black" -> Black
    | _ -> failwith "Error: Unknown Piece color."
  in
  { piece_type = p_type; color = col }

let get_piece_type p =
  match p.piece_type with
  | King -> "King"
  | Queen -> "Queen"
  | Rook -> "Rook"
  | Bishop -> "Bishop"
  | Knight -> "Knight"
  | Pawn -> "Pawn"

let get_color p =
  match p.color with
  | White -> "White"
  | Black -> "Black"

let valid_piece_pos (pos : int * int) =
  match pos with
  | row, col ->
      if row >= 0 && row <= 7 && col >= 0 && col <= 7 then true else false

let valid_pawn_move (p : piece) (pos : int * int) =
  if not (valid_piece_pos pos) then failwith "Invalid piece position."
  else
    match pos with
    | row, col -> (
        let case1 = row < 7 && col > 0 && col < 7 in
        let case2 = row < 7 && col = 0 in
        let case3 = row < 7 && col = 7 in
        match p.color with
        | White ->
            if case1 then
              List.filter valid_piece_pos
                [
                  (row + 1, col);
                  (row + 1, col + 1);
                  (row + 1, col - 1);
                  (row + 2, col);
                ]
            else if case2 then
              List.filter valid_piece_pos
                [ (row + 1, col); (row + 1, col + 1); (row + 2, col) ]
            else if case3 then
              List.filter valid_piece_pos
                [ (row + 1, col); (row + 1, col - 1); (row + 2, col) ]
            else []
        | Black ->
            if case1 then
              List.filter valid_piece_pos
                [
                  (row - 1, col);
                  (row - 1, col + 1);
                  (row - 1, col - 1);
                  (row - 2, col);
                ]
            else if case2 then
              List.filter valid_piece_pos
                [ (row - 1, col); (row - 1, col + 1); (row - 2, col) ]
            else if case3 then
              List.filter valid_piece_pos
                [ (row - 1, col); (row - 1, col - 1); (row - 2, col) ]
            else [])

let vertical_horizontal_moves_aux (pos : int * int) : (int * int) list =
  match pos with
  | row, col ->
      let vertical_moves_down = List.init row (fun i -> (row - i - 1, col)) in
      let vertical_moves_up =
        List.init (7 - row) (fun i -> (row + i + 1, col))
      in
      let horizontal_moves_left = List.init col (fun i -> (row, col - i - 1)) in
      let horizontal_moves_right =
        List.init (7 - col) (fun i -> (row, col + i + 1))
      in
      vertical_moves_down @ vertical_moves_up @ horizontal_moves_left
      @ horizontal_moves_right

let valid_rook_move (pos : int * int) =
  if not (valid_piece_pos pos) then failwith "Invalid piece position."
  else
    match pos with
    | row, col -> vertical_horizontal_moves_aux pos

let valid_knight_move (pos : int * int) =
  if not (valid_piece_pos pos) then failwith "Invalid piece position."
  else
    match pos with
    | row, col ->
        let possible_moves =
          [
            (row + 2, col + 1);
            (row + 1, col + 2);
            (row - 1, col + 2);
            (row - 2, col + 1);
            (row - 2, col - 1);
            (row - 1, col - 2);
            (row + 1, col - 2);
            (row + 2, col - 1);
          ]
        in
        List.filter valid_piece_pos possible_moves

let diagonal_moves_aux (pos : int * int) : (int * int) list =
  match pos with
  | row, col ->
      let moves_diagonal_up_right =
        List.init
          (min (7 - row) (7 - col))
          (fun i -> (row + i + 1, col + i + 1))
      in
      let moves_diagonal_up_left =
        List.init (min (7 - row) col) (fun i -> (row + i + 1, col - i - 1))
      in
      let moves_diagonal_down_right =
        List.init (min row (7 - col)) (fun i -> (row - i - 1, col + i + 1))
      in
      let moves_diagonal_down_left =
        List.init (min row col) (fun i -> (row - i - 1, col - i - 1))
      in
      moves_diagonal_up_right @ moves_diagonal_up_left
      @ moves_diagonal_down_right @ moves_diagonal_down_left

let valid_bishop_move (pos : int * int) =
  if not (valid_piece_pos pos) then failwith "Invalid piece position."
  else
    match pos with
    | row, col -> diagonal_moves_aux pos

let valid_queen_move (pos : int * int) =
  if not (valid_piece_pos pos) then failwith "Invalid piece position."
  else
    match pos with
    | row, col -> vertical_horizontal_moves_aux pos @ diagonal_moves_aux pos

let valid_king_move (pos : int * int) =
  if not (valid_piece_pos pos) then failwith "Invalid piece position."
  else
    match pos with
    | row, col ->
        let possible_moves =
          [
            (row + 1, col);
            (row + 1, col + 1);
            (row, col + 1);
            (row - 1, col + 1);
            (row - 1, col);
            (row - 1, col - 1);
            (row, col - 1);
            (row + 1, col - 1);
          ]
        in
        List.filter valid_piece_pos possible_moves

let piece_to_string (piece : piece option) : string =
  match piece with
  | Some { piece_type = Pawn; color = White } -> "P"
  | Some { piece_type = Knight; color = White } -> "N"
  | Some { piece_type = Bishop; color = White } -> "B"
  | Some { piece_type = Rook; color = White } -> "R"
  | Some { piece_type = Queen; color = White } -> "Q"
  | Some { piece_type = King; color = White } -> "K"
  | Some { piece_type = Pawn; color = Black } -> "P"
  | Some { piece_type = Knight; color = Black } -> "N"
  | Some { piece_type = Bishop; color = Black } -> "B"
  | Some { piece_type = Rook; color = Black } -> "R"
  | Some { piece_type = Queen; color = Black } -> "Q"
  | Some { piece_type = King; color = Black } -> "K"
  | None -> ""

let valid_piece_move (p : piece) (pos : int * int) =
  if not (valid_piece_pos pos) then failwith "Invalid piece position."
  else
    match p.piece_type with
    | Pawn -> valid_pawn_move p pos
    | Rook -> valid_rook_move pos
    | Knight -> valid_knight_move pos
    | Bishop -> valid_bishop_move pos
    | Queen -> valid_queen_move pos
    | King -> valid_king_move pos
