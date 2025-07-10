# --- 1. GAME SETUP ---

# For reproducibility, setting a "seed" ensures the ship is in the
# same "random" location every time the script is run.
set.seed(660)

# Create the game board, a simple 3x3 grid represented by numbers 1 through 9.
grid_size <- 9
game_board <- 1:grid_size

# Randomly select one square to hide the ship.
ship_location <- sample(game_board, 1)

# Set the game parameters.
total_tries <- 5
tries_left <- total_tries
guesses_made <- c() # An empty vector to keep track of guesses

# --- 2. GAMEPLAY LOOP ---

# Welcome message to the player.
cat("Welcome to Hide and Seek!\n")
cat("A ship is hidden in one of the squares from 1 to 9.\n")
cat(paste("You have", total_tries, "tries to find it. Good luck!\n\n"))

# The game continues as long as the player has tries left.
while (tries_left > 0) {
  
  # Print the current status.
  cat(paste("Tries remaining:", tries_left, "\n"))
  cat(paste("You have already guessed:", paste(guesses_made, collapse = ", "), "\n"))
  
  # Prompt the user for their guess and convert it to an integer.
  guess <- as.integer(readline(prompt = "Which square will you check? (1-9): "))
  
  # --- Input Validation ---
  # Check if the guess is a valid number and within the grid.
  if (is.na(guess) || guess < 1 || guess > 9) {
    cat("Invalid input. Please enter a number between 1 and 9.\n\n")
    next # Skip the rest of the loop and start the next try.
  }
  
  # Check if the square has already been guessed.
  if (guess %in% guesses_made) {
    cat("You've already guessed that square! Try another one.\n\n")
    next
  }
  
  # Record the valid guess.
  guesses_made <- c(guesses_made, guess)
  
  # --- Check for a Hit or Miss ---
  if (guess == ship_location) {
    cat(paste("\n*** HIT! *** You found the ship at square", ship_location, "!\n"))
    cat("Congratulations, you win!\n")
    break # Exit the `while` loop because the game is over.
  } else {
    cat("--- MISS! --- The ship is not there.\n\n")
  }
  
  # Decrement the number of tries left.
  tries_left <- tries_left - 1
}

# --- 3. END OF GAME ---

# If the loop finishes and the ship hasn't been found, the player loses.
if (tries_left == 0) {
  cat("\n--- GAME OVER ---\n")
  cat("You've run out of tries!\n")
  cat(paste("The ship was hidden at square", ship_location, ".\n"))
}
