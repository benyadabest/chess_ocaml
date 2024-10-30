type piece_type
(** Represents the type of a chess piece, either King, Queen, Rook, Bishop,
    Knight, Pawn. *)

type color
(** Represents the color of a piece, either White or Black. *)

type piece
(** Represents a chess piece with a type and color. *)

val legal_moves : piece -> int * int -> (int * int) list
(** Returns the legal moves for a given piece on the chessboard. [piece] is the
    chess piece. [pos] is the current position of the piece in the form (row,
    col). Returns a list of valid positions [(row, col)] the piece can move to. *)

val generate_rook_moves : int * int -> (int * int) list
(** Returns the legal moves for a rook on the chess board. [pos] is the current
    position of the piece in the form (row, col). Returns a list of valid
    positions [(row, col)] the piece can move to. *)

val generate_knight_moves : int * int -> (int * int) list
(** Returns the legal moves for a knight on the chess board. [pos] is the
    current position of the piece in the form (row, col). Returns a list of
    valid positions [(row, col)] the piece can move to. *)

val generate_bishop_moves : int * int -> (int * int) list
(** Returns the legal moves for a bishop on the chess board. [pos] is the
    current position of the piece in the form (row, col). Returns a list of
    valid positions [(row, col)] the piece can move to. *)

val generate_queen_moves : int * int -> (int * int) list
(** Returns the legal moves for a queen on the chess board. [pos] is the current
    position of the piece in the form (row, col). Returns a list of valid
    positions [(row, col)] the piece can move to. *)

val generate_king_moves : int * int -> (int * int) list
(** Returns the legal moves for a king on the chess board. [pos] is the current
    position of the piece in the form (row, col). Returns a list of valid
    positions [(row, col)] the piece can move to. *)

val generate_pawn_moves : int * int -> (int * int) list
(** Returns the legal moves for a pawn on the chess board. [pos] is the current
    position of the piece in the form (row, col). Returns a list of valid
    positions [(row, col)] the piece can move to. *)
