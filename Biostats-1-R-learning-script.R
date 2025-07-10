# BIOSTATS 1: Introduction to R for Vision Science
# This script provides an introduction to R programming and syntax for novices,
# with a focus on applications in vision science.

# --- 1. SETUP ---

# It's good practice to start with a clean environment.
# The following command removes all objects from your current R session.
rm(list = ls())

# We'll be using several packages. The 'tidyverse' is a collection of packages
# for data science. 'psych' is useful for psychological research, and 'pwr'
# is for power analysis.
# You only need to install packages once.
# install.packages("tidyverse")
# install.packages("psych")
# install.packages("pwr")

# Load the packages for this session.
library(tidyverse)
library(psych)
library(pwr)


# --- 2. R AS A CALCULATOR ---

# R can perform basic arithmetic.
5 + 3
10 / 2

# You can assign values to variables using the `<-` operator.
# Let's imagine we're working with contrast values.
weber_contrast <- (120 - 100) / 100
michelson_contrast <- (120 - 100) / (120 + 100)

# R is case-sensitive.
Contrast <- 0.5
contrast <- 0.1

# R has many built-in mathematical functions.
log10(2.0) # log base 10
sqrt(16) # square root
round(3.14159, 2) # round to 2 decimal places

# Use `?` to get help on any function.
?log10


# --- 3. DATA STRUCTURES ---

# The most common data structures are vectors and data frames (or tibbles).

## Vectors
# A vector is a one-dimensional sequence of data elements of the same type.
reaction_times_ms <- c(250, 275, 260, 300, 280)
conditions <- c("High Contrast", "Low Contrast")

## Tibbles (Modern Data Frames)
# A tibble is a two-dimensional data structure where columns can have different types.
# This is how you'll usually store your experimental data.
# Let's create a tibble with some hypothetical visual acuity data.
acuity_data <- tribble(
  ~participant, ~age, ~logMAR, ~condition,
  "P01", 22, -0.1, "Treated",
  "P02", 25, 0.0, "Control",
  "P03", 45, 0.2, "Treated",
  "P04", 50, 0.3, "Control",
  "P05", 24, -0.05, "Treated",
  "P06", 48, 0.25, "Control"
)

# You can access a column using the `$` operator.
acuity_data$logMAR


# --- 4. DATA WRANGLING WITH DPLYR ---

# `dplyr` provides intuitive "verbs" for data manipulation.

# `filter()`: Select rows based on a condition.
# Let's find participants in the "Treated" group.
treated_group <- filter(acuity_data, condition == "Treated")

# `select()`: Choose specific columns.
participant_acuity <- select(acuity_data, participant, logMAR)

# `mutate()`: Create new columns.
# Let's convert logMAR to an approximate Snellen fraction.
acuity_data_with_snellen <- mutate(acuity_data,
                                   snellen_denominator = 20 * 10^logMAR)

# `summarize()`: Collapse a data frame to a single row of summary statistics.
# Let's get the mean and standard deviation of logMAR acuity.
acuity_summary <- summarize(acuity_data,
                            mean_logMAR = mean(logMAR),
                            sd_logMAR = sd(logMAR))


## The Pipe Operator: `%>%`
# The pipe operator chains commands together, making your code easier to read.
# It takes the output of the left side and "pipes" it as the first input to the right side.

# Let's get the mean logMAR for participants over 40.
mean_acuity_over_40 <- acuity_data %>%
  filter(age > 40) %>%
  summarize(mean_logMAR = mean(logMAR))


# --- 5. DATA VISUALIZATION WITH GGPLOT2 ---

# `ggplot2` is a powerful package for creating plots based on the "Grammar of Graphics."

# A scatter plot of age vs. logMAR acuity.
ggplot(data = acuity_data, aes(x = age, y = logMAR)) +
  geom_point(size = 3, aes(color = condition)) +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "Visual Acuity Declines with Age",
       x = "Age (years)",
       y = "logMAR Acuity",
       color = "Group") +
  theme_minimal()

# A boxplot comparing the two conditions.
ggplot(data = acuity_data, aes(x = condition, y = logMAR, fill = condition)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(title = "Visual Acuity by Treatment Group",
       x = "Condition",
       y = "logMAR Acuity") +
  theme_minimal() +
  theme(legend.position = "none")


# --- 6. FUNDAMENTAL PROGRAMMING CONCEPTS ---

## `if-else` Statements
# These allow your code to make decisions.
participant_age <- 65
if (participant_age >= 60) {
  print("This participant might be at higher risk for AMD.")
} else {
  print("This participant is not in the high-risk age group for AMD.")
}

## `for` Loops
# Loops repeat a block of code. While `tidyverse` functions often avoid explicit loops,
# understanding them is fundamental.

# Let's convert each logMAR value to a Snellen denominator individually.
snellen_denominators <- c() # create an empty vector
for (i in 1:nrow(acuity_data)) {
  snellen_denominators[i] <- 20 * 10^acuity_data$logMAR[i]
}


# --- 7. BASIC STATISTICAL TESTS ---

## t-test
# Let's compare the logMAR acuity between the "Treated" and "Control" groups.
# The `t.test()` function can take a formula as input.
t_test_result <- t.test(logMAR ~ condition, data = acuity_data)
print(t_test_result)

## Correlation
# Is there a correlation between age and logMAR acuity?
correlation_result <- cor.test(~ age + logMAR, data = acuity_data)
print(correlation_result)


# --- 8. BLOCK RANDOMIZATION ---

# The `psych` package has a simple function for block randomization.
# This is useful for ensuring balanced conditions in an experiment.
# Let's create a trial list for 40 trials with two conditions ("Left Tilt", "Right Tilt").
n_trials <- 40
n_conditions <- 2
conditions <- c("Left Tilt", "Right Tilt")

randomized_list <- block.random(n = n_trials,
                                ncond = n_conditions,
                                cond.names = conditions)

# Let's view the first 10 trials.
head(randomized_list, 10)


# --- 9. POWER ANALYSIS ---

# Power analysis helps you determine the sample size needed to detect an effect.
# Let's calculate the required sample size for our t-test example.
# We'll assume a medium effect size (Cohen's d = 0.5) and desire 80% power.

power_analysis_result <- pwr.t.test(d = 0.5,
                                    sig.level = 0.05,
                                    power = 0.80,
                                    type = "two.sample")
print(power_analysis_result)

# The result indicates we need approximately 64 participants per group... more on this later.
