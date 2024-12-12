type square = Pieces.piece option
(** Represents a square on the board, which may or may not contain a piece *)

type board = square array array
(** Represents the chessboard as an 8x8 grid. *)

val init_board : unit -> board
(** Initializes the board with pieces in their starting positions. *)

val get_piece_at : board -> int * int -> square
(** Returns the piece at a given position. Raises [Invalid_position] if position is invalid. *)

val move_piece : board -> int * int -> int * int -> board
(** Moves a piece from one position to another. Raises [Invalid_move] if move is invalid. *)

val is_valid_position : int * int -> bool
(** Checks if the given position is valid on the board *)

val algebraic_to_indices : string -> int * int
(** Converts algebraic notation (e.g., "e4") to array indices *)

val indices_to_algebraic : int * int -> string
(** Converts array indices to algebraic notation *)

val move_piece_algebraic : board -> string -> string -> board
(** Moves a piece using algebraic notation *)

val print_board : board -> unit
(** Prints the current state of the board *)

val is_valid_move : board -> int * int -> int * int -> bool
(** Checks if a move from start position to end position is valid *)

val is_check : board -> string -> bool
(** [is_check board color] checks if the king of the given color is in check *)

val is_checkmate : board -> string -> bool
(** [is_checkmate board color] checks if the given color is in checkmate *)