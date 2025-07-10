# BIOSTATS 1: R for Optometry - Data Analysis and Statistical Testing
# This script covers data import, cleaning, visualization, and common
# statistical tests using examples relevant to clinical optometry.

# --- 1. SETUP ---
# It's good practice to start with a clean environment.
# This command removes all objects (data, variables, etc.) from your R session.
rm(list = ls())

# We will use several packages today. You only need to run the install.packages()
# lines once on your machine. If they are already installed, you can skip them.
# install.packages("tidyverse")
# install.packages("readxl") # For reading Excel files
# install.packages("broom")    # For tidying model output
# install.packages("pwr")      # For power analysis
# install.packages("lsr")      # For effect sizes

# Load the packages for this session. This is done every time you start R.
library(tidyverse)
library(readxl)
library(broom)
library(pwr)
library(lsr)


# --- 2. IMPORTING & CREATING CLINICAL DATA ---
# While you can create data frames by hand, you will usually import data
# from a file, such as a CSV or Excel spreadsheet.

# Let's create a sample patient dataset using a tibble (a modern data frame).
# This is great for making small, reproducible examples.
patient_data <- tribble(
  ~patient_id, ~age, ~sex, ~iop_od, ~cct_od, ~treatment,
  "P01", 68, "M", 23, 545, "Latanoprost",
  "P02", 71, "F", 18, 560, "Placebo",
  "P03", 59, "F", 25, 530, "Latanoprost",
  "P04", 65, "M", 19, 555, "Placebo",
  "P05", 74, "F", 28, 520, "Latanoprost",
  "P06", 62, "M", 20, 550, "Placebo"
)

# You can inspect your data by printing its name.
print(patient_data)

# You can access a single column (variable) using the `$` operator.
# Let's look at all the right eye IOP (Intraocular Pressure) measurements.
patient_data$iop_od


# --- 3. DATA CLEANING & WRANGLING WITH `dplyr` ---
# We often need to modify our data for analysis. The `dplyr` package
# provides intuitive "verbs" for data manipulation.

# `filter()`: Select rows based on a condition.
# Let's find patients older than 70.
patients_over_70 <- patient_data %>%
  filter(age > 70)

# `mutate()`: Create or modify columns.
# Let's adjust IOP for corneal thickness (CCT). A common rule of thumb is to
# adjust IOP by 1 mmHg for every 20 microns away from 545 microns.
iop_adjusted <- patient_data %>%
  mutate(iop_od_adjusted = iop_od - (cct_od - 545) / 20)

# `summarize()`: Calculate summary statistics.
# Let's find the mean and standard deviation for age and IOP.
data_summary <- patient_data %>%
  summarize(
    mean_age = mean(age),
    sd_age = sd(age),
    mean_iop = mean(iop_od),
    n_patients = n() # n() is a handy function to count rows
  )

print(data_summary)


# --- 4. VISUALIZING CLINICAL DATA WITH `ggplot2` ---
# `ggplot2` is the standard for creating flexible and beautiful plots in R.

# Let's create a boxplot comparing IOP between the two treatment groups.
ggplot(data = patient_data, aes(x = treatment, y = iop_od, fill = treatment)) +
  geom_boxplot(alpha = 0.8) +
  geom_jitter(width = 0.1, alpha = 0.6) + # Adds individual data points
  labs(
    title = "IOP Comparison Between Treatment Groups",
    x = "Treatment Group",
    y = "Right Eye IOP (mmHg)",
    fill = "Group"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Let's create a scatter plot to visualize the relationship between CCT and IOP.
ggplot(data = patient_data, aes(x = cct_od, y = iop_od)) +
  geom_point(size = 3, color = "dodgerblue") +
  geom_smooth(method = "lm", color = "firebrick") + # Adds a linear regression line
  labs(
    title = "IOP vs. Central Corneal Thickness",
    x = "CCT (microns)",
    y = "IOP (mmHg)"
  ) +
  theme_minimal()


# --- 5. STATISTICAL HYPOTHESIS TESTING ---
# R has a comprehensive suite of functions for statistical testing.

## Independent Samples t-test
# Let's test for a significant difference in mean IOP between our treatment groups.
t_test_result <- t.test(iop_od ~ treatment, data = patient_data)

# The `broom` package cleans up the output into a tidy data frame.
tidy(t_test_result)

## Chi-squared Test for Proportions
# Is there an association between sex and treatment group assignment?
contingency_table <- table(patient_data$sex, patient_data$treatment)
print(contingency_table)

chisq.test(contingency_table)


## One-Way ANOVA (Analysis of Variance)
# ANOVA is used to compare the means of three or more groups.
# Let's add a third drug, "Timolol", to our dataset for this example.
patient_data_anova <- patient_data %>%
  bind_rows(
    tribble(
      ~patient_id, ~age, ~sex, ~iop_od, ~cct_od, ~treatment,
      "P07", 66, "M", 21, 540, "Timolol",
      "P08", 70, "F", 22, 535, "Timolol"
    )
  )

# Perform the one-way ANOVA.
anova_model <- aov(iop_od ~ treatment, data = patient_data_anova)
tidy(anova_model) # `tidy()` gives the ANOVA table

# If the ANOVA is significant, use Tukey's HSD for post-hoc pairwise comparisons.
tidy(TukeyHSD(anova_model))


# --- 6. EFFECT SIZE & POWER ANALYSIS ---

## Effect Size
# A p-value tells you if there's an effect, but not how large it is.
# For a t-test, Cohen's d is a common effect size measure.
# Let's calculate it for our original t-test.
cohens_d_result <- cohen.d(iop_od ~ treatment, data = patient_data)
print(cohens_d_result)
# An effect size of d > 0.8 is typically considered large.

# For ANOVA, eta-squared (η²) is a common effect size.
etaSquared(anova_model)
# This value represents the proportion of variance in IOP explained by the treatment.


## Power Analysis
# Power analysis helps determine the sample size needed to detect an effect.
# Let's find the required sample size for a new study comparing two eye drops.
# We expect a medium effect size (d = 0.5) and want 80% power.
power_t_test <- pwr.t.test(
  d = 0.5,
  sig.level = 0.05,
  power = 0.80,
  type = "two.sample"
)
print(power_t_test)
# The result for 'n' is the required sample size *per group*.


# --- 7. REGRESSION ANALYSIS ---
# Regression helps us model the relationship between a continuous outcome
# and one or more predictors.

# Can we predict IOP based on a patient's age and corneal thickness?
regression_model <- lm(iop_od ~ age + cct_od, data = patient_data)

# The `summary()` function provides detailed model results.
summary(regression_model)

# The `broom` package provides a tidy summary of the coefficients.
tidy(regression_model)
# This table gives you the estimate, standard error, t-statistic, and p-value
# for each predictor in the model (including the intercept).

# It's good practice to start with a clean environment.
# This command removes all objects (data, variables, etc.) from your R session.
rm(list = ls())

# We will use several packages today. Please note that you only need to run the installation.packages()
# lines once on your machine. If they are already installed, you can skip them.
# install.packages("tidyverse")
# install.packages("readxl") # For reading Excel files
# install.packages("broom")    # For tidying model output
# install.packages("pwr")      # For power analysis
# install.packages("lsr")      # For effect sizes

# Load the packages for this session. This is done every time you start R.
library(tidyverse)
library(readxl)
library(broom)
library(pwr)
library(lsr)


# --- 2. IMPORTING & CREATING CLINICAL DATA ---
# While you can create data frames by hand, you will usually import data
# from a file, such as a CSV or Excel spreadsheet.

# Let's create a sample patient dataset using a tibble (a modern data frame).
# This is great for making small, reproducible examples.
patient_data <- tribble(
  ~patient_id, ~age, ~sex, ~iop_od, ~cct_od, ~treatment,
  "P01", 68, "M", 23, 545, "Latanoprost",
  "P02", 71, "F", 18, 560, "Placebo",
  "P03", 59, "F", 25, 530, "Latanoprost",
  "P04", 65, "M", 19, 555, "Placebo",
  "P05", 74, "F", 28, 520, "Latanoprost",
  "P06", 62, "M", 20, 550, "Placebo"
)

# You can inspect your data by printing its name.
print(patient_data)

# You can access a single column (variable) using the `$` operator.
# Let's look at all the right eye IOP (Intraocular Pressure) measurements.
patient_data$iop_od


# --- 3. DATA CLEANING & WRANGLING WITH `dplyr` ---
# We often need to modify our data for analysis. The `dplyr` package
# provides intuitive "verbs" for data manipulation.

# `filter()`: Select rows based on a condition.
# Let's find patients older than 70.
patients_over_70 <- patient_data %>%
  filter(age > 70)

# `mutate()`: Create or modify columns.
# Let's adjust IOP for corneal thickness (CCT). A common rule of thumb is to
# Adjust IOP by one mm/Hg for every 20 microns away from 545 microns.
iop_adjusted <- patient_data %>%
  mutate(iop_od_adjusted = iop_od - (cct_od - 545) / 20)

# `summarize()`: Calculate summary statistics.
# Let's find the mean and standard deviation for age and IOP.
data_summary <- patient_data %>%
  summarize(
    mean_age = mean(age),
    sd_age = sd(age),
    mean_iop = mean(iop_od),
    n_patients = n() # n() is a handy function to count rows
  )

print(data_summary)


# --- 4. VISUALIZING CLINICAL DATA WITH `ggplot2` ---
# `ggplot2` is the standard for creating flexible and beautiful plots in R.

# Let's create a boxplot comparing IOP between the two treatment groups.
ggplot(data = patient_data, aes(x = treatment, y = iop_od, fill = treatment)) +
  geom_boxplot(alpha = 0.8) +
  geom_jitter(width = 0.1, alpha = 0.6) + # Adds individual data points
  labs(
    title = "IOP Comparison Between Treatment Groups",
    x = "Treatment Group",
    y = "Right Eye IOP (mmHg)",
    fill = "Group"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Let's create a scatter plot to visualize the relationship between CCT and IOP.
ggplot(data = patient_data, aes(x = cct_od, y = iop_od)) +
  geom_point(size = 3, color = "dodgerblue") +
  geom_smooth(method = "lm", color = "firebrick") + # Adds a linear regression line
  labs(
    title = "IOP vs. Central Corneal Thickness",
    x = "CCT (microns)",
    y = "IOP (mmHg)"
  ) +
  theme_minimal()


# --- 5. STATISTICAL HYPOTHESIS TESTING ---
# R has a comprehensive suite of functions for statistical testing.

## Independent Samples t-test
# Let's test for a significant difference in mean IOP between our treatment groups.
t_test_result <- t.test(iop_od ~ treatment, data = patient_data)

# The `broom` package cleans up the output into a tidy data frame.
tidy(t_test_result)

## Chi-squared Test for Proportions
# Is there an association between sex and treatment group assignment?
contingency_table <- table(patient_data$sex, patient_data$treatment)
print(contingency_table)

chisq.test(contingency_table)


## One-Way ANOVA (Analysis of Variance)
# ANOVA is used to compare the means of three or more groups.
# Let's add a third drug, "Timolol", to our dataset for this example.
patient_data_anova <- patient_data %>%
  bind_rows(
    tribble(
      ~patient_id, ~age, ~sex, ~iop_od, ~cct_od, ~treatment,
      "P07", 66, "M", 21, 540, "Timolol",
      "P08", 70, "F", 22, 535, "Timolol"
    )
  )

# Perform the one-way ANOVA.
anova_model <- aov(iop_od ~ treatment, data = patient_data_anova)
tidy(anova_model) # `tidy()` gives the ANOVA table

# If the ANOVA is significant, use Tukey's HSD for post-hoc pairwise comparisons.
tidy(TukeyHSD(anova_model))


# --- 6. EFFECT SIZE & POWER ANALYSIS ---

## Effect Size
# A p-value tells you if there's an effect, but not how large it is.
# For a t-test, Cohen's d is a common effect size measure.
# Let's calculate it for our original t-test.
cohens_d_result <- cohen.d(iop_od ~ treatment, data = patient_data)
print(cohens_d_result)
# An effect size of d > 0.8 is typically considered large.

# For ANOVA, eta-squared (η²) is a common effect size.
etaSquared(anova_model)
# This value represents the proportion of variance in IOP explained by the treatment.


## Power Analysis
# Power analysis helps determine the sample size needed to detect an effect.
# Let's find the required sample size for a new study comparing two eye drops.
# We expect a medium effect size (d = 0.5) and want 80% power.
power_t_test <- pwr.t.test(
  d = 0.5,
  sig.level = 0.05,
  power = 0.80,
  type = "two.sample"
)
print(power_t_test)
# The result for 'n' is the required sample size *per group*.


# --- 7. REGRESSION ANALYSIS ---
# Regression helps us model the relationship between a continuous outcome
# and one or more predictors.

# Can we predict IOP based on a patient's age and corneal thickness?
regression_model <- lm(iop_od ~ age + cct_od, data = patient_data)

# The `summary()` function provides detailed model results.
summary(regression_model)

# The `broom` package provides a tidy summary of the coefficients.
tidy(regression_model)
# This table gives you the estimate, standard error, t-statistic, and p-value
# for each predictor in the model (including the intercept).
