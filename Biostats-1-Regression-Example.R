#### Simple Regression Example - Field et al (2012) ####

#install.packages('car')
#install.packagaes('QuantPsyc')
album1 <- read.delim("http://www.discoveringstatistics.com/docs/Album%20Sales%202.dat", header = TRUE)

# The line below will fail, but it provides you with the general form of the linear model function. The linear model is behind many/most of the Frequentist tests you will use on your data.
newModel <- lm(fomula = outcome ~ predictor, data = dataFrame)

#newModel is an object that can be summarized with the summary() function which will give you the results of the inferential test.

#If we apply this to our example data...
albumSales.1 <- lm(sales ~ adverts, data = album1)

# Summarize our simple regression model...
(modelSummary <- summary(albumSales.1))

# We can check for the indivudal variables within our regression model. 
ls(modelSummary)
sqrt(modelSummary$r.squared)

#### multiple regression ####

# We can compute the simple restricted model first as a baseline.
albumSales.2 <- glm(sales ~ adverts, data = album1)
# Now we can grow the model to include multiple predictors.
albumSales.3 <- glm(sales ~ adverts + airplay + attract, data = album1)

# Now compare the summaries of the two models...
summary(albumSales.2)
summary(albumSales.3)

#### using the bootstrap ####

library(boot)

bootReg <- function(formula, data, i) {
  d <- data[i,]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

bootResults <- boot(statistic = bootReg, formula = sales ~ adverts + airplay, data = album1, R = 1000)

boot.ci(bootResults, type = "bca", index = 1) # intercept
boot.ci(bootResults, type = "bca", index = 2) # adverts
boot.ci(bootResults, type = "bca", index = 3) # airplay
#### Simple Regression Example - Field et al (2012) ####

#install.packages('car')
#install.packagaes('QuantPsyc')
album1 <- read.delim("http://www.discoveringstatistics.com/docs/Album%20Sales%202.dat", header = TRUE)

# The line below will fail, but it provides you with the general form of the linear model function. The linear model is behind many/most of the Frequentist tests you will use on your data.
newModel <- lm(fomula = outcome ~ predictor, data = dataFrame)

#newModel is an object that can be summarized with the summary() function which will give you the results of the inferential test.

#If we apply this to our example data...
albumSales.1 <- lm(sales ~ adverts, data = album1)

# Summarize our simple regression model...
(modelSummary <- summary(albumSales.1))

# We can check for the indivudal variables within our regression model. 
ls(modelSummary)
sqrt(modelSummary$r.squared)

#### multiple regression ####

# We can compute the simple restricted model first as a baseline.
albumSales.2 <- glm(sales ~ adverts, data = album1)
# Now we can grow the model to include multiple predictors.
albumSales.3 <- glm(sales ~ adverts + airplay + attract, data = album1)

# Now compare the summaries of the two models...
summary(albumSales.2)
summary(albumSales.3)

#### using the bootstrap ####

library(boot)

bootReg <- function(formula, data, i) {
  d <- data[i,]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

bootResults <- boot(statistic = bootReg, formula = sales ~ adverts + airplay, data = album1, R = 1000)

boot.ci(bootResults, type = "bca", index = 1) # intercept
boot.ci(bootResults, type = "bca", index = 2) # adverts
boot.ci(bootResults, type = "bca", index = 3) # airplay
