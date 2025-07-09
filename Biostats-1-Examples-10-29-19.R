#### OPTOMETRY STATISTICS LAB - FREQUENTIST ANALYSIS ####

# Simulate refractive error and screen time
set.seed(2025)
n <- 100
screen_time <- rnorm(n, mean = 6, sd = 2)  # Hours per day
se <- rnorm(n, mean = -0.5 - 0.25 * screen_time, sd = 1)

refraction_data <- data.frame(
  SE = se,
  ScreenHours = screen_time
)

# Simple linear regression: SE ~ screen time
myopia_model <- lm(SE ~ ScreenHours, data = refraction_data)
summary(myopia_model)


#### MULTIPLE REGRESSION: Predict logMAR acuity ####

refraction_data$Age <- rnorm(n, mean = 24, sd = 4)
refraction_data$ContrastSens <- rnorm(n, mean = 1.8, sd = 0.3)

# Simulated visual acuity based on predictors
refraction_data$logMAR <- with(refraction_data,
  0.1 - 0.15 * SE + 0.2 * (2 - ContrastSens) + 0.005 * Age + rnorm(n, 0, 0.1)
)

# Multiple regression
acuity_model <- lm(logMAR ~ SE + ContrastSens + Age, data = refraction_data)
summary(acuity_model)


#### BOOTSTRAP CONFIDENCE INTERVALS ####

library(boot)

bootReg <- function(formula, data, i) {
  d <- data[i, ]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

boot_results <- boot(statistic = bootReg,
                     formula = logMAR ~ SE + ContrastSens,
                     data = refraction_data, R = 1000)

boot.ci(boot_results, type = "bca", index = 1)  # Intercept
boot.ci(boot_results, type = "bca", index = 2)  # SE
boot.ci(boot_results, type = "bca", index = 3)  # ContrastSens


#### COHEN'S D EFFECT SIZE: Dry Eye Study ####

library(effsize)
library(lsr)

treated <- rnorm(30, mean = 15, sd = 3)
control <- rnorm(30, mean = 18, sd = 3)

# t-test
dryeye_test <- t.test(treated, control)

# Cohen's d
cohen.d(treated, control)


#### ANOVA AND ETA SQUARED: Lens Designs ####

lens_group <- rep(c("LensA", "LensB", "LensC"), each = 20)
VA <- c(
  rnorm(20, mean = 0.1, sd = 0.05),
  rnorm(20, mean = 0.08, sd = 0.05),
  rnorm(20, mean = 0.12, sd = 0.05)
)

lens_data <- data.frame(VA, LensType = factor(lens_group))

# ANOVA model
anova_model <- aov(VA ~ LensType, data = lens_data)
summary(anova_model)

# Effect size
etaSquared(anova_model)

# Post-hoc
TukeyHSD(anova_model)


#### POWER ANALYSIS ####

library(pwr)

# Power of t-test
pwr.t.test(n = 48, d = 0.5, sig.level = 0.05, type = "two.sample")

# Power for ANOVA
pwr.anova.test(k = 3, n = 20, f = 0.2, sig.level = 0.05)

# Required N for power = 0.8
pwr.anova.test(power = 0.8, k = 3, f = 0.2, sig.level = 0.05)


#### LOGISTIC REGRESSION: Myopia Onset ####

refraction_data$Myopia <- as.factor(ifelse(refraction_data$SE < -0.50, 1, 0))

log_model <- glm(Myopia ~ ScreenHours + Age, data = refraction_data, family = binomial)
summary(log_model)

# Predicted probabilities
myopia_prob <- predict(log_model, type = "response")
head(myopia_prob)
