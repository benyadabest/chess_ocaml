type piece_type
(** Represents the type of a chess piece, either King, Queen, Rook, Bishop,
    Knight, Pawn. *)

type color
(** Represents the color of a piece, either White or Black. *)

type piece
(** Represents a chess piece with a type and color. *)

val create_piece : string -> string -> piece
(** Creates a chess piece with a specified type and color. [p_type] is the type
    of the chess piece (e.g., King, Queen, Pawn). [col] is the color of the
    piece, either White or Black. Returns a [piece] record with the given type
    and color. *)

val piece_to_int : piece option -> int
(** Converts a piece to its integer representation. *)

val get_piece_type : piece -> string
(** Returns the type of the piece as a string. *)

val get_color : piece -> string
(** Returns the color of the piece as a string. *)
