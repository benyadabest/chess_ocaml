(*
Key Responsibilities of the game Module:
Game State Management:
The game module maintains the overall state of the game, including the current board, which player's turn it is, and whether the game is over.
It will keep track of any special states (e.g., castling rights, en passant possibilities, check status).
Turn Management:
This module handles alternating turns between the two players (White and Black).
After each move, it updates the game state, switches the player turn, and checks for game-ending conditions (like checkmate or stalemate).
Move Validation:
The game module validates moves made by the players by interacting with the pieces and board modules. It checks:
Whether the move is valid for the piece in question (based on its movement rules).
Whether the move is legal in terms of the overall game (e.g., ensuring no piece moves through other pieces except for the knight, preventing the player from moving into check).
Special Rules:
Chess has several special rules (castling, en passant, pawn promotion), and the game module is responsible for enforcing these rules.
Check and Checkmate Detection:
The game module detects when a king is in check or checkmate by checking all possible moves and whether any move can remove the threat to the king.
It might also check for a stalemate condition when no legal moves are available for the current player, but the king is not in check.
Game Flow:
The game module controls the sequence of events:
Prompting the user for input (via the ui module).
Validating the move.
Making changes to the board and piece positions.
Checking for game-ending conditions.
Moving to the next player's turn.


*)