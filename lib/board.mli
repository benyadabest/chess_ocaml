type square = Pieces.piece option
(** Represents a square on the board, which may or may not contain a piece *)

type board = square array array
(** Represents the chessboard as an 8x8 grid. *)

val init_board : unit -> board
(** Initializes the board with pieces in their starting positions. *)

val get_piece_at : board -> int * int -> square
(** Returns the piece at a given position, or None if the position is invalid or empty. *)

val move_piece : board -> int * int -> int * int -> board
(** Moves a piece from one position to another. Raises [Invalid_move] if move is invalid. *)

val is_valid_position : int * int -> bool
(** Checks if the given position is valid on the board. *)

val algebraic_to_indices : string -> int * int
(** Converts algebraic notation to array indices. *)

val indices_to_algebraic : int * int -> string
(** Converts array indices to algebraic notation. *)

val move_piece_algebraic : board -> string -> string -> board
(** Moves a piece using algebraic notation. Raises [Invalid_move] if move is invalid. *)

val print_board : board -> unit
(** Prints the current state of the board. *)

val is_valid_move : board -> int * int -> int * int -> bool
(** Checks if a move from start position to end position is valid, considering the board state. *)

val valid_piece_move : board -> Pieces.piece -> int * int -> (int * int) list
(** Determines valid moves for a specific piece at the given position,
    considering board state and rules for the piece's movement. *)

val is_check : board -> string -> bool
(** Checks if the king of the given color is in check. *)

val is_checkmate : board -> string -> bool
(** Checks if the king of the given color is in checkmate. *)

val is_stalemate : board -> string -> bool
(** [is_stalemate board color] returns true if the player of the given color
      is in stalemate (not in check but has no legal moves). *)

val place_piece : board -> string -> Pieces.piece -> unit
(** [place_piece board position piece] places [piece] at the given [position] on the [board].
    The [position] is specified in algebraic notation. Raises [Invalid_argument] if the [position] is invalid. *)

