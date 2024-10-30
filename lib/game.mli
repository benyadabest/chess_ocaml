type game_state
(** Represents the state of the game. *)

val init_game : unit -> game_state
(** Initializes a new game. Returns the initial game state. *)

val make_move : game_state -> int * int -> int * int -> game_state * string
(** Makes a move in the game. [game_state] is the current state of the game.
    [start_pos] is the (row, col) of the piece to move. [end_pos] is the (row,
    col) where the piece is moving to. Returns the updated game state and a
    result indicating the status of the game. Raises a `Failure` if [start_pos]
    or [end_pos] is not a valid coordinate on the board, or if the move from
    [start_pos] to [emd_pos] is not a valid move. *)

val game_over : game_state -> bool
(** Checks if the game has ended. Returns true if the game is over (checkmate,
    stalemate, or draw), false otherwise. *)

val is_whites_turn : game_state -> bool
(** Checks whether or not it's white's turn to move. *)
