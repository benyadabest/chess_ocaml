type square = Pieces.piece option
(** Represents a square on the board, which may or may not contain a piece. *)

type board = square array array
(** Represents the chessboard as an 8x8 grid. *)

val init_board : unit -> board
(** Initializes the board with pieces in their starting positions. *)

val get_piece_at : board -> int * int -> square
(** Returns the piece at a given position, or None if the position is invalid or
    empty. *)

val is_valid_position : int * int -> bool
(** Checks if the given position is valid on the board (row and col must be
    between 0 and 7). *)

val is_empty_square : board -> int * int -> bool
(** Returns true if the given position on the board is empty (None), false
    otherwise. *)

val can_castle_kingside : board -> string -> bool
(** Checks if castling kingside is possible for the given [color] ("White" or
    "Black"). *)

val do_castle_kingside : board -> string -> board
(** Performs kingside castling for the given [color]. Raises [Failure] if the
    castling move is invalid. *)

val valid_pawn_moves : board -> Pieces.piece -> int * int -> (int * int) list
(** Returns a list of valid moves for a pawn, considering captures and
    two-square moves from its start position. *)

val sliding_moves :
  board ->
  Pieces.piece ->
  int * int ->
  int * int ->
  (int * int) list ->
  (int * int) list
(** [sliding_moves board piece (start_row, start_col) (delta_row, delta_col) acc]
    recursively computes all valid squares along a sliding direction (e.g. for
    Rook/Bishop/Queen). [acc] is the accumulator for discovered valid positions. *)

val is_valid_move : board -> int * int -> int * int -> bool
(** Checks if a move from [start_pos] to [end_pos] is valid, considering the
    board state. *)

val valid_piece_move : board -> Pieces.piece -> int * int -> (int * int) list
(** Determines valid moves for a specific piece at the given position,
    considering [board] state and the piece's movement rules. *)

val promote_pawn : board -> int -> int -> unit
(** Promotes a pawn at [row, col] to a queen if it has reached the last rank. *)

val move_piece : board -> int * int -> int * int -> board
(** Moves a piece from [start_pos] to [end_pos]. Raises [Failure "Invalid move"]
    if the move is invalid. *)

val find_king : board -> string -> int * int
(** Finds the position of the king belonging to [color]. Raises [Failure] if no
    king is found. *)

val is_check : board -> string -> bool
(** Checks if the king of the given color is in check on the given [board]. *)

val is_checkmate : board -> string -> bool
(** Checks if the king of the given color is in checkmate on the given [board]. *)

val is_stalemate : board -> string -> bool
(** Returns true if the given [color] is not in check but has no legal moves
    (stalemate). *)

val algebraic_to_indices : string -> int * int
(** Converts algebraic notation like "e4" to (row, col). Raises
    [Failure "Invalid notation format"] or [Failure "Invalid position"] if
    notation or position is invalid. *)

val indices_to_algebraic : int * int -> string
(** Converts array indices (row, col) back to algebraic notation like "e4". *)

val move_piece_algebraic : board -> string -> string -> board
(** Moves a piece using algebraic notation ([start_pos], [end_pos]). Raises
    [Failure "Invalid move"] if move is invalid. *)

val print_board : board -> unit
(** Prints the current state of the board to stdout. *)

val place_piece : board -> string -> Pieces.piece -> unit
(** [place_piece board position piece] places [piece] at the given [position] in
    algebraic notation. Raises [Failure "Invalid position"] if the [position] is
    outside the board. *)
