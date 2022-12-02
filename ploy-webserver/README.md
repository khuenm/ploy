# Start server
- to run the server, you need python3
- go the the folder of the webserver and run:
  - python ploy.py
- open website 127.0.0.1:8000 in browser

# Validation and playing against bot
In order to play against the bot or use the validation (based on the bot),
you have to create an executable Haskell bot using the command "stack build".
This creates a binary (.exe under windows, executable file under unix).
The path to the binary needs to be stored in the variable PATH_TO_BOT in ploy.py

# Supported Game modes
The ploy server supports three gaming modes:
1. "without validation": A move from the GUI is not checked. Any move will be executed on the board.
2. "with validation": A move is checked by the bot implementation. Therefore, the executable needs to be build an embedded (see above).
3. "against haskell bot": Moves are checked similar to "with validation". The white player is based on te bot implementation.

# Additional information
The console of the browser will give additional information on the current FEN string.
In addition, the instantiation of the board as the Haskell type [[Cell]] is given.