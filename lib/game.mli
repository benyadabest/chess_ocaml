type game_state

val init_game : unit -> game_state
(** Initializes a new game and returns the initial [game_state]. *)

val get_board : game_state -> Board.board
(** Returns the current board from the given [game_state]. *)

val game_over : game_state -> bool
(** Returns true if the game is over. *)

val is_whites_move : game_state -> bool
(** Returns true if it is currently White's turn. *)

val current_player_color : game_state -> string
(** Returns "White" if it's White's turn, "Black" otherwise. *)

val print_board : Board.board -> unit
(** Prints the chessboard with spacing. *)

val update_game_status : game_state -> game_state
(** Updates the game state by checking check, checkmate, etc. *)

val move : game_state -> int * int -> int * int -> game_state
(** Moves a piece from start to end positions. Raises an exception if invalid. *)

val set_error_message : game_state -> string -> game_state
(** Sets the error message in the game state. *)

val clear_error_message : game_state -> game_state
(** Clears the error message in the game state. *)

val get_error_message : game_state -> string option
(** Retrieves the current error message, if any. *)
