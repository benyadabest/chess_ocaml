(** Represents a square on the board, which can either be empty or occupied by a piece. *)
type square = (Pieces.piece option)

(** Represents the chessboard as an 8x8 grid. *)
type board = square array array

(** Initializes the board with pieces in their starting positions. *)
val init_board : unit -> board

(** Moves a piece from one position to another.
    [board] is the current board state.
    [start_pos] is the (row, col) of the piece to move.
    [end_pos] is the (row, col) where the piece is moving to.
    Returns the updated board. *)
val move_piece : board -> (int * int) -> (int * int) -> board

(** Returns the piece at a given position. *)
val get_piece_at : board -> (int * int) -> Pieces.piece option