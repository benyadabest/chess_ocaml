type piece_type
(** Represents the type of a chess piece, either King, Queen, Rook, Bishop,
    Knight, Pawn. *)

type color
(** Represents the color of a piece, either White or Black. *)

type piece
(** Represents a chess piece with type [piece_type] and color [color]. *)

val create_piece : string -> string -> piece
(** Creates a chess piece with a specified type and color. [p_type] is the type
    of the chess piece (e.g., King, Queen, Pawn). [col] is the color of the
    piece, either White or Black. Returns a [piece] record with the given type
    and color. *)

val piece_to_int : piece option -> int
(** [piece_to_int piece option] converts a piece to its integer representation. *)

val get_piece_type : piece -> string
(** [get_piece_type piece] returns the type of the piece as a string. *)

val get_color : piece -> string
(** [get_color piece] returns the color of the piece as a string. *)

val valid_pawn_move : piece -> int * int -> (int * int) list
(** [valid_pawn_move p pos] returns a list of valid pawn moves for a specified
    pawn on the board. *)

val valid_rook_move : piece -> int * int -> (int * int) list
(** [valid_rook_move p pos] returns a list of valid rook moves for a specified
    rook on the board. *)

val valid_knight_move : piece -> int * int -> (int * int) list
(** [valid_knight_move p pos] returns a list of valid knight moves for a
    specified knight on the board. *)

val valid_bishop_move : piece -> int * int -> (int * int) list
(** [valid_bishop_move p pos] returns a list of valid bishop moves for a
    specified bishop on the board. *)

val valid_queen_move : piece -> int * int -> (int * int) list
(** [valid_queen_move p pos] returns a list of valid queen moves for a specified
    queen on the board. *)

val valid_king_move : piece -> int * int -> (int * int) list
(** [valid_knight_move p pos] returns a list of valid knight moves for a
    specified knight on the board. *)
