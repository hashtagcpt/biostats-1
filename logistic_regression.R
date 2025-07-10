# Title: A Teaching Example for Predictive Modeling
# Topic: Why Training and Testing on the Same Data is a Bad Idea
# This script demonstrates a common mistake in modeling and how to correct it.

# --- SETUP ---
# The 'ISLR' package contains the 'Smarket' dataset with S&P 500 data.
# You only need to install a package once.
# install.packages("ISLR")
library(ISLR)

# Load the Smarket dataset into the environment.
data(Smarket)


# --- PART 1: A FLAWED (BUT COMMON) APPROACH ---
#
# KEY TEACHING POINT: This example intentionally uses the entire dataset for
# both training and testing the model. This is a common beginner's mistake
# that leads to an overly optimistic and unrealistic measure of performance.

# We'll use logistic regression because the outcome ('Direction') is binary (Up/Down).
# `glm` stands for Generalized Linear Model.
# `family = binomial` specifies logistic regression.
# The formula `Direction ~ Lag1 + ...` means "predict Direction based on these variables".
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               data = Smarket,
               family = binomial)

# Use the fitted model to predict the probability of the market going "Up" for each day.
# `type = "response"` outputs the probabilities (e.g., 0.52) instead of log-odds.
glm.probs <- predict(glm.fit, type = "response")

# Now, we convert these probabilities into categorical predictions ("Up" or "Down").
# First, create a vector that assumes every prediction is "Down".
glm.pred <- rep("Down", nrow(Smarket))
# Then, for any day where the predicted probability was greater than 50%,
# change the prediction in our vector to "Up".
glm.pred[glm.probs > 0.5] <- "Up"

# A confusion matrix shows how our predictions line up with the actual outcomes.
# The rows are our predictions, and the columns are the true directions.
# It helps us see not just what we got right, but also what we got wrong.
#          Actual
# Predicted Down  Up
#      Down  145 141  <- Correctly predicted "Down" 145 times (True Negatives)
#      Up    457 507  <- Correctly predicted "Up" 507 times (True Positives)
table(glm.pred, Smarket$Direction)

# Calculate the overall accuracy of our model ON THE DATA IT WAS TRAINED ON.
# This is (True Positives + True Negatives) / Total Observations.
mean(glm.pred == Smarket$Direction)
# The model achieves about 52% accuracy on this data. While slightly better
# than a coin flip (50%), this result is misleading because the model is
# being tested on the exact same data it learned from. It may have just
# memorized the noise in the data, rather than a real signal.


# --- PART 2: A BETTER APPROACH (TRAIN/TEST SPLIT) ---
#
# To get a realistic measure of a model's predictive power, you must test
# it on UNSEEN data. The standard method is to split your data into a
# training set and a testing set.

# For time series data like this, a common approach is to train the model
# on an earlier period and test it on a later period.
train_data <- Smarket[Smarket$Year < 2005, ] # Data from 2001-2004
test_data <- Smarket[Smarket$Year == 2005, ]  # Data from 2005

# Now, build the model using ONLY the training data.
glm.fit.split <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                     data = train_data,
                     family = binomial)

# Now we use this new model to make predictions on the `test_data`,
# which the model has never seen before. This simulates how the model
# would perform in the real world on new data.
glm.probs.split <- predict(glm.fit.split, newdata = test_data, type = "response")

# Convert the new probabilities to "Up" or "Down" predictions.
glm.pred.split <- rep("Down", nrow(test_data))
glm.pred.split[glm.probs.split > 0.5] <- "Up"

# Create the confusion matrix for the TEST SET.
table(glm.pred.split, test_data$Direction)

# Calculate the accuracy on the unseen test data.
mean(glm.pred.split == test_data$Direction)
# The accuracy on the unseen data is now around 48% — worse than a coin flip!
# This demonstrates that our original model had no real predictive power.
# The 52% accuracy we saw earlier was an illusion caused by overfitting.


# --- PART 3: FURTHER IDEAS & NEXT STEPS ---

# How could this model be improved?
#
# 1. Feature Engineering: Instead of just using lagged returns, you could
#    create more sophisticated predictors. For example, you could calculate a
#    30-day moving average of the volume or a measure of recent volatility.
#
# 2. Model Selection: The original model includes all available predictors.
#    A better approach is to use a more principled method to select only the
#    predictors that have a real relationship with the outcome. This can be
#    done using techniques like stepwise regression or by looking at the p-values
#    of the coefficients in the model summary (`summary(glm.fit.split)`). A simpler
#    model with fewer predictors is often more robust.
