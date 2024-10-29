(** Represents the state of the game. *)
type game_state

(** Initializes a new game. Returns the initial game state. *)
val init_game : unit -> game_state

(** Makes a move in the game.
    [game_state] is the current state of the game.
    [start_pos] is the (row, col) of the piece to move.
    [end_pos] is the (row, col) where the piece is moving to.
    Returns the updated game state and a result indicating the status of the game. *)
val make_move : game_state -> (int * int) -> (int * int) -> game_state * string

(** Checks if the game has ended.
    Returns true if the game is over (checkmate, stalemate, or draw), false otherwise. *)
val game_over : game_state -> bool

(** Retrieves the current player's turn (White or Black). *)
val current_turn : game_state -> Pieces.color