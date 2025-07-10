# --- 1. SETUP ---

# Clear all objects from the current R session to start fresh.
rm(list = ls())

# Set a "seed" for the random number generator. This makes the script
# produce the exact same "random" data every time it's run.
set.seed(32)


# --- 2. DATA SIMULATION ---

# Define the levels for the abstract experimental conditions.
cond_1 <- c('A', 'B', 'C', 'D')
cond_2 <- c('0', '6', '12')

# This function uses nested loops to generate data for a full factorial design.
# NOTE: This version is optimized. Instead of using `rbind()` in the loop
# (which is very slow), it saves each new row to a list and then combines
# them all at once at the end. This is a much more efficient practice in R.
make_data_sim <- function(cond_1, cond_2, n_sub) {
  # 1. Initialize an empty list to store the rows.
  results_list <- list()

  # 2. Loop through each subject ID provided.
  for (n_sub_ctr in n_sub) {
    # 3. Loop through each level of the first condition.
    for (cond_1_ctr in 1:length(cond_1)) {
      # 4. Loop through each level of the second condition.
      for (cond_2_ctr in 1:length(cond_2)) {
        # Create a single row as a vector.
        new_row <- c(
          n_sub_ctr,
          cond_1[cond_1_ctr],
          cond_2[cond_2_ctr],
          rnorm(1, mean = .2, sd = .01) # The random data point
        )
        # Add the new row to our list of results.
        results_list <- append(results_list, list(new_row))
      }
    }
  }

  # 5. Efficiently combine the list of rows into a data frame.
  d <- as.data.frame(do.call(rbind, results_list))
  return(d)
}

# --- Generate data for the first group ---
n_sub <- 1:12
d_em <- make_data_sim(cond_1, cond_2, n_sub)
colnames(d_em) <- c('subject', 'blur', 'ecc', 'thresh')

# Add the between-subjects variable for this group.
re_var <- rep('Em', nrow(d_em))
d_em$re <- re_var

# --- Generate data for the second group ---
n_sub <- 13:24
d_my <- make_data_sim(cond_1, cond_2, n_sub)
colnames(d_my) <- c('subject', 'blur', 'ecc', 'thresh')

# Add the between-subjects variable for this group.
re_var <- rep('My', nrow(d_my))
d_my$re <- re_var

# --- Combine groups and finalize data types ---
d <- rbind(d_em, d_my)

# It's good practice to ensure columns have the correct data type.
# The 'thresh' column was created from mixed data (numbers and characters)
# so we explicitly convert it to numeric for the analysis.
d$thresh <- as.numeric(d$thresh)
d$subject <- as.factor(d$subject)


# --- 3. ANALYSIS ---

# A standalone t-test example comparing two different random distributions.
t.test(rnorm(n = 30, mean = 0.2, sd = 0.05), rt(n = 30, df = 1) + .25)

# Load the 'ez' package for easy ANOVA analysis.
library(ez)

# Run the mixed-design ANOVA.
# dv: dependent variable (what you measured)
# wid: within-subject identifier (the participant ID)
# within: the within-subject factors
# between: the between-subjects factor
ezANOVA(
  data = d,
  dv = thresh,
  wid = subject,
  within = .(blur, ecc),
  between = re,
  detailed = TRUE
)
