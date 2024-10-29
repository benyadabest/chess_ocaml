(** Represents the type of a chess piece. *)
type piece_type = 
  | King 
  | Queen 
  | Rook 
  | Bishop 
  | Knight 
  | Pawn

(** Represents the color of a piece, either White or Black. *)
type color = 
  | White 
  | Black

(** Represents a chess piece with a type and color. *)
type piece = {
  piece_type : piece_type;
  color : color;
}

(** Returns the legal moves for a given piece on the chessboard.
    [piece] is the chess piece.
    [pos] is the current position of the piece in the form (row, col).
    Returns a list of valid positions [(row, col)] the piece can move to. *)
val legal_moves : piece -> (int * int) -> (int * int) list
