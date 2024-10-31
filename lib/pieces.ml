(* Defines types *)
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

let create_piece p c =
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

(** 1 = Pawn, 2 = Knight, 3 = Bishop, 4 = Rook, 5 = Queen, 6 = King *)
let piece_to_int (piece : piece option) : int =
  match piece with
  | Some { piece_type = Pawn; color = White } -> 1
  | Some { piece_type = Knight; color = White } -> 2
  | Some { piece_type = Bishop; color = White } -> 3
  | Some { piece_type = Rook; color = White } -> 4
  | Some { piece_type = Queen; color = White } -> 5
  | Some { piece_type = King; color = White } -> 6
  | Some { piece_type = Pawn; color = Black } -> -1
  | Some { piece_type = Knight; color = Black } -> -2
  | Some { piece_type = Bishop; color = Black } -> -3
  | Some { piece_type = Rook; color = Black } -> -4
  | Some { piece_type = Queen; color = Black } -> -5
  | Some { piece_type = King; color = Black } -> -6
  | None -> 0
