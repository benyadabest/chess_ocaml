type game_state
(** Represents the state of the game. *)

val init_game : unit -> game_state
(** Initializes a new game. Returns the initial game state. *)

val game_over : game_state -> bool
(** Checks if the game has ended. Returns true if the game is over (checkmate,
    stalemate, or draw), false otherwise. *)

val is_whites_move : game_state -> bool
(** Checks whether or not it's white's turn to move. *)

val print_board : game_state -> unit
(** Prints the current board. *)

val move : game_state -> int * int -> int * int -> game_state
(** Moves a piece from [start_row, start_col] to [end_row, end_col]. Returns the
    updated game state. Raises an error if the move is invalid. *)
