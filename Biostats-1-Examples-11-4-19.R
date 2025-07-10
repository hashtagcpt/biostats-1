# Title: R for Statistical Analysis - Effect Size, Power, and Categorical Tests
# Author: [Your Name/Institution]
# Date: [Current Date]
# Description: This script demonstrates how to calculate effect sizes, conduct
#              power analysis, and perform chi-squared tests in R.
#              It is intended for teaching purposes.

# --- Setup: Load Required Packages ---

# The following packages are used in this script.
# You only need to install them once. If you've already installed them,
# you can comment out the install.packages() lines.
# install.packages("effsize")
# install.packages("lsr")
# install.packages("pwr")
# install.packages("MASS") # Often comes with R, but good to be explicit

library(effsize)
library(lsr)
library(pwr)
library(MASS)

# For reproducibility of any random numbers we generate.
# Using set.seed() ensures that we all get the same "random" results.
set.seed(123)

# ---
## Section 1: Effect Size
# ---
# Effect size is a standardized measure of the magnitude of a phenomenon.
# It helps us understand the practical significance of our results beyond
# just the p-value.

#### 1A: Cohen's d (Known Population SD) ####

# Cohen's d measures the difference between two means in units of
# standard deviation.
#
# Scenario: We know the population standard deviation for IQ is 15.
# A new teaching method increases the average IQ by 5 points.
# Let's calculate the effect size.

mu_control <- 100
mu_treatment <- 105 # Changed from 115 to match the 5-point increase described
sigma <- 15

# CORRECTED FORMULA: Cohen's d is the difference in means divided
# by the standard deviation (sigma), NOT the variance (sigma^2).
cohens_d_known <- (mu_treatment - mu_control) / sigma

cat("Cohen's d (known sigma):", cohens_d_known, "\n") # We can interpret d = 0.33 as a small-to-medium effect.

#### 1B: Cohen's d (Unknown Population SD) ####

# Usually, we don't know the population SD. We estimate it from our sample data.
# We can calculate Cohen's d from a t-test result.

# First, let's generate some sample data for three groups.
n_subjects <- 30
group_A <- rnorm(n = n_subjects, mean = 11.5, sd = 2.5)
group_B <- rnorm(n = n_subjects, mean = 10.5, sd = 2.5)
group_C <- rnorm(n = n_subjects, mean = 9.5, sd = 2.5)

# Perform t-tests between the groups.
t_test_AB <- t.test(group_A, group_B)
t_test_AC <- t.test(group_A, group_C)
t_test_BC <- t.test(group_B, group_C)

# --- A Note on Multiple Comparisons ---
# When we run multiple tests, our chance of a false positive increases.
# We should correct our p-values. The Bonferroni and Benjamini-Hochberg (BH)
# methods are common ways to do this.

p_values <- c(t_test_AB$p.value, t_test_AC$p.value, t_test_BC$p.value)
p_adjusted_bonferroni <- p.adjust(p_values, method = 'bonferroni')
p_adjusted_bh <- p.adjust(p_values, method = 'BH')

cat("\nOriginal p-values:", round(p_values, 4), "\n")
cat("Bonferroni adjusted p-values:", round(p_adjusted_bonferroni, 4), "\n")
cat("BH adjusted p-values:", round(p_adjusted_bh, 4), "\n")


# Now, let's calculate Cohen's d for the A vs. B comparison.
# We can do this manually from the t-statistic...
# The formula is: d = t * sqrt(1/n1 + 1/n2)
d_computed_manual <- t_test_AB$statistic * sqrt((1/n_subjects) + (1/n_subjects))
cat("\nManually computed Cohen's d (A vs B):", d_computed_manual, "\n")

# ...or, more easily, using the `effsize` package.
cohens_d_package <- cohen.d(group_A, group_B)
print(cohens_d_package)


#### 1C: Eta-Squared (η²) for ANOVA ####

# For ANOVA, a common effect size is eta-squared (η²). It represents the
# proportion of the total variance that is attributable to an effect.
# Let's use the 'genotype' dataset from the MASS package.

data(genotype)
head(genotype) # Let's see what the data looks like

# We will run a two-way ANOVA to see how 'Mother' and 'Litter' affect
# the weight ('Wt') of the mice.
anova_model <- aov(Wt ~ Mother * Litter, data = genotype) # Using * is a shortcut for main effects + interaction
summary(anova_model)

# The `lsr` package provides a simple function to calculate eta-squared
# for each effect in our ANOVA model.
eta_squared_results <- etaSquared(anova_model)
print(eta_squared_results)
# The output shows the proportion of variance in weight explained by Mother,
# Litter, and their interaction. A value of 0.14 means 14% of the variance
# is explained by that factor.


# ---
## Section 2: Power Analysis
# ---
# Power is the probability of finding a statistically significant result,
# given that a real effect exists. A common target for power is 0.80 (or 80%).
# We use power analysis to determine the sample size needed to detect an effect.

#### 2A: Power for a t-test ####

# Scenario: What is the power of a two-group t-test with 48 subjects PER GROUP,
# a small-to-medium effect size (d=0.3), and an alpha of 0.05?
# NOTE: The 'n' in pwr.t.test is the sample size in *each* group.
power_t_test <- pwr.t.test(n = 48, d = 0.3, sig.level = 0.05, type = "two.sample")
print(power_t_test)
# The result shows power is ~0.45, which is low. We'd likely need more subjects.

#### 2B: Power for ANOVA ####

# Scenario: What is the power of a one-way ANOVA with 4 groups, 12 subjects
# per group, a medium effect size (f=0.25), and an alpha of 0.05?
# Note: Cohen's 'f' is the effect size measure for ANOVA.
# f = 0.1 (small), 0.25 (medium), 0.4 (large).
power_anova <- pwr.anova.test(k = 4, n = 12, f = 0.25, sig.level = .05)
print(power_anova)
# Power here is ~0.35, also quite low.

#### 2C: Calculating Required Sample Size (N) ####

# Scenario: How many subjects per group would we need in the ANOVA above
# to achieve a power of 0.80?
required_n_anova <- pwr.anova.test(k = 4, f = 0.25, sig.level = .05, power = .80)
print(required_n_anova)
# The result shows we need n=45 subjects *per group* (180 total).

#### 2D: Calculating Required Effect Size (f) ####

# Scenario: If we only have 12 subjects per group for our 4-group ANOVA,
# what is the minimum effect size ('f') we could reliably detect with 80% power?
required_f_anova <- pwr.anova.test(k = 4, n = 12, sig.level = .05, power = .80)
print(required_f_anova)
# The result shows we would need to be looking for a very large effect (f=0.53)
# to have decent power with such a small sample.


# ---
## Section 3: Categorical Variables (Chi-Squared Test)
# ---
# The Chi-Squared (χ²) test is used to analyze categorical data. It tells us
# if the observed frequencies in categories differ significantly from the
# expected frequencies.

#### 3A: Goodness-of-Fit (Manual Calculation) ####

# Scenario: At a shelter, we expect to adopt out 40% dogs and 60% cats.
# This means for every 40 adoptions, we expect 16 dogs and 24 cats.
# In one month, 22 dogs and 18 cats were adopted. Do these observations
# deviate significantly from our expectation?

# Expected and Observed Frequencies
observed_counts <- c(22, 18) # 22 dogs, 18 cats
expected_props <- c(0.4, 0.6) # 40% dogs, 60% cats
total_adoptions <- sum(observed_counts) # 40

expected_counts <- total_adoptions * expected_props # 16 dogs, 24 cats

# The Chi-Squared statistic (χ²) is calculated as: Σ [ (Observed - Expected)² / Expected ]
chi_squared_manual <- sum((observed_counts - expected_counts)^2 / expected_counts)
cat("\nManually calculated Chi-Squared statistic (χ²):", chi_squared_manual, "\n")

# To get a p-value, we compare our statistic to the Chi-Squared distribution.
# The degrees of freedom (df) is the number of categories minus 1. Here, df = 2 - 1 = 1.
# We look at the probability of getting a value as extreme or more extreme than ours.
p_value_manual <- pchisq(chi_squared_manual, df = 1, lower.tail = FALSE)
cat("P-value from manual calculation:", p_value_manual, "\n")


#### 3B: Goodness-of-Fit (Using R's Function) ####

# R's built-in `chisq.test()` makes this much easier.
# We provide the observed counts and the expected probabilities.
chi_test_results <- chisq.test(x = observed_counts, p = expected_props)
print(chi_test_results)
# The result gives the same χ² value (X-squared) and p-value.


#### 3C: Goodness-of-Fit (More Complex Example) ####

# Scenario: On a 5-option multiple-choice test, 150 students answered a question.
# If they were guessing randomly, we'd expect an equal number for each option (20%).
# The observed choices were: A=63, B=65, C=10, D=10, E=2.

student_answers <- c(63, 65, 10, 10, 2)
random_guessing_probs <- c(0.2, 0.2, 0.2, 0.2, 0.2) # equal probability

chisq.test(x = student_answers, p = random_guessing_probs)
# The extremely small p-value confirms students were not guessing randomly.

# Now, let's test a more specific hypothesis. The instructor believed the
# answer probabilities were: A=35%, B=35%, C=14%, D=14%, E=2%.
instructor_hyp_probs <- c(0.35, 0.35, 0.14, 0.14, 0.02)

chisq.test(x = student_answers, p = instructor_hyp_probs)

# The result is still significant. This means the observed student responses
# do not fit the instructor's expected model either. The warning message
# "Chi-squared approximation may be incorrect" appears because one of the
# expected counts is very low (150 * 0.02 = 3). This is R letting us know
# the test's assumption might be violated, but in this case, the result is
# so significant it doesn't change our conclusion.
