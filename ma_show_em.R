# --- 1. SETUP ---
# It's good practice to start with a clean environment.
rm(list = ls())

# Load packages for plotting and data manipulation.
library(ggplot2)
library(tidyverse)


# --- 2. PLOTTING EXAMPLE ---
# This section generates two sets of random data and plots them.

# Generate 1001 random numbers from a standard normal distribution for x and y.
# Note: data_x and data_y are independent; there is no relationship between them.
data_x <- rnorm(1001)
data_y <- rnorm(1001)

# Create two separate, evenly spaced sequences of numbers.
x <- seq(-3, 3, .001 * 6)
y <- seq(-3, 3, .001 * 6)

# Combine the random data into a tibble (a modern data frame).
ta_da <- tibble(data_x, data_y)

# Create a scatter plot of the random data_x and data_y points.
# Then, stat_smooth attempts to overlay a logistic regression curve.
# NOTE: This is an unusual use case, as the line is being fit to the
# separate 'x' and 'y' sequences, not the random data shown in the plot.
ta_da %>% ggplot(aes(x = data_x, y = data_y)) +
  geom_point() +
  stat_smooth(aes(x = x, y = y), method = "glm", family = "binomial")


# --- 3. BAYESIAN REASONING EXAMPLE ---
# This section defines an interactive card game to demonstrate Bayesian updating.
# To play the game, you must call the function in the console: runCardBayes()

runCardBayes <- function() {
  # This nested function handles reading user input for the card color.
  readcolor <- function() {
    n <- readline(prompt = "Enter color of the card removed (R or B) : ")
    n <- toupper(n)
    # Recursively asks for input until it gets a valid character.
    if (is.na(n) || ((n != 'R') && (n != 'B') && (n != 'Q'))) {
      n <- readcolor()
    }
    return(n)
  }
  
  # This function applies Bayes' rule to update a prior probability.
  bayesrule <- function(prior, conditional1, conditional2) {
    posterior <- (prior * conditional1) / ((prior * conditional1) + (1 - prior * conditional2))
  }
  
  # Set the initial (prior) probabilities.
  pRedRemoved <- 0.5
  pBlackRemoved <- 0.5
  
  # Define the conditional probabilities.
  conditionalRemoved <- 13 / 39 # Prob of drawing red, given a red suit was removed.
  conditionalNotRemoved <- 26 / 39 # Prob of drawing red, given a black suit was removed.
  
  stopComputing <- FALSE
  
  # The main game loop, continues until the user enters 'Q' to quit.
  while (stopComputing != TRUE) {
    card_color <- readcolor()
    if (card_color != 'Q') {
      # Update probabilities based on the color of the card drawn.
      if (card_color == 'R') {
        pRedRemoved <- bayesrule(pRedRemoved, conditionalRemoved, conditionalNotRemoved)
        pBlackRemoved <- bayesrule(pBlackRemoved, conditionalNotRemoved, conditionalRemoved)
      } else {
        pRedRemoved <- bayesrule(pRedRemoved, conditionalNotRemoved, conditionalRemoved)
        pBlackRemoved <- bayesrule(pBlackRemoved, conditionalRemoved, conditionalNotRemoved)
      }
      # Calculate and print the updated probabilities and the Bayes Factor.
      bayesfactor_k <- pRedRemoved / pBlackRemoved
      print(sprintf("The probability that red was removed is %f", pRedRemoved))
      print(sprintf("The probability that black was removed is %f", pBlackRemoved))
      print(sprintf("Bayes Factor (K) for (H = red removed) is %f", bayesfactor_k))
    } else {
      # End the game if the user enters 'Q'.
      stopComputing <- TRUE
    }
  }
  return()
}


# --- 4. TEXAS HOLD'EM SIMULATION ---
# This section defines a function to simulate a single hand of Texas Hold'em.
# NOTE: This function depends on a 'deck' object existing in the global environment.

# A deck of 52 cards (Jokers are excluded).
deck = c("A", "A", "A", "A", "K", "K", "K", "K", "Q", "Q", "Q", "Q", "J", "J", "J", "J", 10, 10, 10, 10, 9, 9, 9, 9, 8, 8, 8, 8, 7, 7, 7, 7, 6, 6, 6, 6, 5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2)

# This function simulates a hand for 2 players and the dealer.
# It modifies the global 'deck' variable as it runs.
hand <- function() {
  # Deal Player 1's hand.
  player1Hand <- sample(deck, 2, F)
  # This loop removes the dealt cards from the global deck.
  for (i in player1Hand) {
    if (is.element(i, deck)) {
      deck <- deck[-match(i, deck)]
    }
  }
  
  # Deal Player 2's hand.
  player2Hand <- sample(deck, 2, F)
  # This loop removes the dealt cards from the global deck.
  for (i in player2Hand) {
    if (is.element(i, deck)) {
      deck <- deck[-match(i, deck)]
    }
  }
  
  # Deal the flop (first three community cards).
  dealt_cards <- sample(deck, 3, F)
  # This loop removes the dealt cards from the global deck.
  for (i in dealt_cards) {
    if (is.element(i, deck)) {
      deck <- deck[-match(i, deck)]
    }
  }
  
  # Deal the turn (4th community card).
  second_dealt <- sample(deck, 1, F)
  # This loop removes the card and adds it to the community cards.
  for (i in second_dealt) {
    if (is.element(i, deck)) {
      deck <- deck[-match(i, deck)]
      dealt_cards = append(dealt_cards, second_dealt)
    }
  }
  
  # Deal the river (5th community card).
  third_dealt <- sample(deck, 1, F)
  # This loop removes the card and adds it to the community cards.
  for (i in third_dealt) {
    if (is.element(i, deck)) {
      deck <- deck[-match(i, deck)]
      dealt_cards = append(dealt_cards, third_dealt)
    }
  }
  
  # A list of the results is returned by the function.
  results = list(player1Hand, player2Hand, dealt_cards)
  return(results)
}
