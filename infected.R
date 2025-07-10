# --- SCRIPT SETUP ---

# Clear all objects from the R session to start fresh.
rm(list = ls())

# Load the tidyverse package, which is useful for data manipulation and plotting.
library(tidyverse)

# Set a "seed" for the random number generator. This makes the script's
# random outcomes reproducible every time it's run.
set.seed(123)


### Example 1: The "Infected!" Simulation Game 🎲
# This script runs a simple interactive game to simulate a "test" for infection.
# The player's chosen number is tested against a random value.

# --- THE SIMULATION FUNCTION ---
# This function calculates a single "test score".
doTheTest <- function(player_result, how_many_digits) {
  # The "test" involves a set of dice. We get a random outcome for each.
  d4   <- sample(1:4)
  d6   <- sample(1:6)
  d8   <- sample(1:8)
  d10  <- sample(1:10)
  d12  <- sample(1:12)
  d20  <- sample(1:20)
  d100 <- sample(seq(10, 100, by = 10))
  
  # The denominator is a complex random value based on the dice rolls.
  random_denominator <- sum(
    d4[1:how_many_digits] / 4,
    d6[1:how_many_digits] / 6,
    d8[1:how_many_digits] / 8,
    d10[1:how_many_digits] / 10,
    d12[1:how_many_digits] / 12,
    d20[1:how_many_digits] / 20,
    d100[1:how_many_digits] / 100
  )
  
  # The final score is a ratio of the player's result to the total.
  score <- player_result / (player_result + random_denominator)
  return(score)
}

# --- RUN THE SIMULATION ---
## Game Parameters
how_many_rolls <- 10 # How many times we run the test to get a stable median.
how_many_digits <- 4 # Complexity of the random value (must be between 1 and 4).
detection_cutoff <- 0.5 # The score needed to be considered "infected".

## User Input
cat("--- Welcome to the 'Infected!' Simulation ---\n")
name_infected <- readline(prompt = "Enter the name of the person being tested: ")
player_result <- as.integer(readline(prompt = "Enter a test result (a whole number, e.g., 5): "))

# This vector will store the score from each roll.
test_scores <- c()

cat("\n--- Running Simulation ---\n")
# This loop runs the test `how_many_rolls` times.
for (i in 1:how_many_rolls) {
  score <- doTheTest(player_result, how_many_digits)
  test_scores[i] <- score
  cat(sprintf("Run %d: Score = %.4f\n", i, score))
  Sys.sleep(0.1) # Pause briefly to make the simulation easier to watch.
}

# --- GET THE FINAL RESULT ---
final_median_score <- median(test_scores)

cat("\n--- Final Diagnosis ---\n")
cat(sprintf("The median test score for %s was %.4f\n", name_infected, final_median_score))
cat(sprintf("The detection cutoff is %.2f\n", detection_cutoff))

# Compare the final score to the detection cutoff.
if (final_median_score >= detection_cutoff) {
  cat(sprintf("\nResult: %s is INFECTED. Game over.\n", name_infected))
} else {
  cat(sprintf("\nResult: %s is CLEAR. You are safe for now!\n"))
}

# Add a separator for clarity between the two examples.
cat("\n\n-------------------------------------------------\n\n")


### Example 2: Simulating and Comparing Different Dice 🎲
# This script simulates a large number of rolls for different types of dice
# (d4, d6, etc.) to explore their statistical properties.

# --- THE SIMULATION ---
# Define the types of dice and how many times to roll each one.
dice_types <- c(4, 6, 8, 10, 12, 20)
n_rolls <- 10000

# Use `map_dfr` to run a simulation for each die type and combine the
# results into a single, clean data frame.
all_rolls_data <- map_dfr(dice_types, ~{
  tibble(
    die = paste0("d", .x),
    roll = sample(1:.x, size = n_rolls, replace = TRUE)
  )
})

# --- SUMMARIZING THE RESULTS ---
# Calculate the mean and standard deviation for each type of die.
dice_summary <- all_rolls_data %>%
  group_by(die) %>%
  summarize(
    mean_roll = mean(roll),
    sd_roll = sd(roll)
  )

# Print the summary table.
# The factor levels are reordered to ensure a logical d4 -> d20 order.
print(
    dice_summary %>%
    mutate(die = fct_relevel(die, "d4", "d6", "d8", "d10", "d12", "d20")) %>%
    arrange(die)
)

# --- VISUALIZING THE DISTRIBUTIONS ---
# A density plot shows the shape of each die's distribution.
# A single die roll has a flat (uniform) distribution.
ggplot(all_rolls_data, aes(x = roll, fill = die)) +
  geom_density(alpha = 0.5) +
  # `facet_wrap` creates a separate plot for each die type.
  facet_wrap(~fct_relevel(die, "d4", "d6", "d8", "d10", "d12", "d20")) +
  labs(
    title = "Probability Distributions of Different Sided Dice",
    subtitle = "Each die type has a unique uniform distribution.",
    x = "Die Roll Outcome",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = "none") # Hide the legend as the facets are labeled.
