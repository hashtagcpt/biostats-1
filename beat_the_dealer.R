# BIOSTATS 1: R for Optometry - Simulation Examples
# This script demonstrates two types of simulations in R:
# 1. A clinically relevant model of myopia progression.
# 2. A fun, corrected simulation of a Blackjack-like "hit until you bust" game.

# --- 1. SETUP ---
# It's good practice to start with a clean environment.
rm(list = ls())

# Load the tidyverse package for data manipulation and plotting.
library(tidyverse)

# For reproducibility, we set a "seed." This ensures that the "random" numbers
# we generate will be the same every time we run the script.
set.seed(222)


# --- Example 1: Simulating Myopia Progression (Clinical Simulation) ---

## SETUP: SIMULATION PARAMETERS ##
n_patients <- 1000 # Number of patients in our simulation
sim_years <- 5      # How many years we will simulate progression for

# Baseline Clinical Parameters (Year 0)
mean_baseline_se <- -1.50 # Average starting prescription (in Diopters)
sd_baseline_se <- 0.75    # Variability in the starting prescription

# Progression & Treatment Parameters
mean_progression_rate <- -0.60 # Average progression per year without treatment
sd_progression_rate <- 0.20     # How much progression varies between patients
treatment_efficacy <- 0.40      # Treatment slows progression by 40%
high_myopia_threshold <- -6.00  # Clinical threshold for high myopia


## THE SIMULATION: A TIDYVERSE APPROACH ##
# We generate our entire dataset at once for speed and clarity.
myopia_study_data <- tibble(
  patient_id = 1:n_patients,
  group = if_else(patient_id <= n_patients / 2, "Treatment", "Control"),
  baseline_se = rnorm(n_patients, mean = mean_baseline_se, sd = sd_baseline_se),
  annual_progression = rnorm(n_patients, mean = mean_progression_rate, sd = sd_progression_rate)
) %>%
mutate(
  final_progression_rate = if_else(group == "Treatment",
                                   annual_progression * (1 - treatment_efficacy),
                                   annual_progression),
  final_se = baseline_se + (final_progression_rate * sim_years),
  is_high_myope = if_else(final_se <= high_myopia_threshold, "Yes", "No")
)


## ANALYSIS & VISUALIZATION (MYOPIA) ##
# We can use `group_by()` and `summarize()` to get key metrics for each group.
clinical_outcomes <- myopia_study_data %>%
  group_by(group) %>%
  summarize(
    number_of_patients = n(),
    avg_final_se = mean(final_se),
    prevalence_high_myopia = mean(is_high_myope == "Yes") * 100
  )

# Print the summary table.
print(clinical_outcomes)

# Visualize the distribution of final prescriptions.
ggplot(myopia_study_data, aes(x = final_se, fill = group)) +
  geom_histogram(binwidth = 0.25, alpha = 0.7, position = "identity") +
  geom_vline(xintercept = high_myopia_threshold, linetype = "dashed", color = "black", size = 1) +
  scale_fill_manual(values = c("Treatment" = "#0072B2", "Control" = "#D55E00")) +
  labs(
    title = "Distribution of Final Refractive Error after 5 Years",
    subtitle = "Comparing Myopia Treatment vs. Control Groups",
    x = "Final Spherical Equivalent (Diopters)",
    y = "Number of Patients"
  ) +
  theme_minimal()

---

# --- Example 2: "Hit Until You Bust" Game (Corrected Logic) ---

## SETUP: GAME PARAMETERS & FUNCTIONS ##
# Define the card deck. Ace is 11, face cards are 10.
cards <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10, 11)

# Function to play a single hand according to the original "hit until bust" rule.
play_one_hand_original_logic <- function() {
  # Start with an initial two-card hand.
  current_score <- sum(sample(cards, 2, replace = TRUE))
  num_hits <- 0

  # This `while` loop continues to add cards as long as the score is 21 or less.
  # Once the score is over 21, the loop terminates and the hand is a "bust".
  # This now correctly reflects the logic of your original script.
  while (current_score <= 21) {
    new_card <- sample(cards, 1, replace = TRUE)
    current_score <- current_score + new_card
    num_hits <- num_hits + 1
  }
  
  # This simulates a very rare event happening during the hand.
  # The t-distribution with df=1 is very wide, so drawing a value > 6 is rare.
  rare_event_occurred <- if_else(rt(1, df = 1) > 6, 1, 0)

  # Return the results of the hand as a named list.
  return(list(
    final_score = current_score,
    hits_taken = num_hits,
    rare_event = rare_event_occurred
  ))
}

## THE SIMULATION: A MODERN R APPROACH ##
# We'll play the game 10,000 times.
n_hands <- 10000

# `purrr::map_dfr()` runs our function `n_hands` times and stacks the
# results into a clean data frame. This replaces the original `for` loop.
game_data <- map_dfr(1:n_hands, ~play_one_hand_original_logic())

## ANALYSIS & VISUALIZATION (GAME) ##
# Let's look at the first few hands played.
head(game_data)

# Calculate summary statistics for our game results.
game_summary <- game_data %>%
  summarize(
    avg_bust_score = mean(final_score),
    avg_hits_per_hand = mean(hits_taken),
    rare_event_percentage = mean(rare_event == 1) * 100
  )

# Print the game summary.
print(game_summary)

# Visualize the distribution of the final "bust" scores.
ggplot(game_data, aes(x = final_score)) +
  geom_histogram(binwidth = 1, fill = "#D55E00", color = "white") +
  geom_vline(xintercept = 22, linetype = "dashed", color = "black", size = 1,
             aes(caption="Lowest possible bust score")) +
  labs(
    title = "Distribution of Final Scores in 'Hit Until You Bust' Game",
    subtitle = "All scores are 'busts' (greater than 21)",
    x = "Final Score",
    y = "Number of Hands"
  ) +
  theme_minimal()
