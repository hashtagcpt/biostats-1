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

#### Effect Size ####

# Cohen's d is a measure of effect size -- for a known variance, we can calculate it from the difference in means and a known standard deviation. For IQ, we know that the standard deviation is 15 and if we have a manipulation that increases IQ by 5 points, we can calculate Cohen's d as follows.

mu1 <- 100
mu2 <- 115
sigma <- 15

# Cohen's d
d <- (mu2 - mu1) / sigma^2
print(d)

# For an unknown variance we can also calculate cohen's d, using known quantities from our sample and t-test...

n_subjects_1 <- 30
n_subjects_2 <- 30
sample_data_1 <- rnorm(n = n_subjects_1, mean = 11.5, sd = 1.5)
sample_data_2 <- rnorm(n = n_subjects_2, mean = 10.5, sd = 1.5)
sample_data_3 <- rnorm(n = n_subjects_2, mean = 9.5, sd = 1.5)

example.t <- t.test(sample_data_1, sample_data_2)
example.t

example.t13 <- t.test(sample_data_1, sample_data_3)
example.t13

example.t23 <- t.test(sample_data_2, sample_data_3)
example.t23

p.adjust(c(example.t$p.value, example.t13$p.value, example.t23$p.value), method = 'bonferroni')

p.adjust(c(example.t$p.value, example.t13$p.value, example.t23$p.value), method = 'BH')

d.computed <- example.t$statistic * sqrt(1/n_subjects_1 + 1/n_subjects_2)
d.computed

#install.packages('effsize')
#install.packages('lsr')
library(effsize)
library(lsr)

cohend <- cohen.d(sample_data_1,sample_data_2)
print(cohend)

d <- c(sample_data_1,sample_data_2)
f <- rep(c("data_1",'data_2'), each = 30)

# For the ANOVA things get a little bit more complicated, but we can use the function etaSquared from the lsr package. This function works on an ANOVA object like we saw in previous class. Let's load in genotype from MASS package and perform a two-way ANOVA.

library(MASS)
data(genotype)

# assign the genotype to a data frame for convenience
dat <- genotype

# get our anova model
anova.genotype <- aov(data = dat, Wt ~ Litter + Mother + Litter:Mother)
anova.genotype <- aov(data = dat, Wt ~ Litter*Mother)

# Post-hoc TukeyHSD
TukeyHSD(anova.genotype)

# summarize the ANOVA model
summary(anova.genotype)

# check to see the effect size of Mother
etaSquared(anova.genotype)

#### Power Analysis ####
install.packages('pwr')
library(pwr)

# what is the power of a two-group t-test with a moderate effect size and an alpha of .05?
pwr.t.test(n=48*2,d=0.2,sig.level=0.05)

# what is the power of a four variable moderate effect size anova?
pwr.anova.test(k=4, n=12, f=0.2, sig.level=.05)

# we can use this to calculate a required N
pwr.anova.test(power = .25, k=4, f=0.2, sig.level=.05)

# what's the ball park of the effect size we need to be looking at?
pwr.anova.test(power = .25, k=4, n=12, sig.level=.05)


#### Power Analysis ####
install.packages('pwr')
library(pwr)

# what is the power of a two-group t-test with a moderate effect size and an alpha of .05?
pwr.t.test(n=48*2,d=0.2,sig.level=0.05)

# what is the power of a four variable moderate effect size anova?
pwr.anova.test(k=4, n=12, f=0.2, sig.level=.05)

# we can use this to calculate a required N
pwr.anova.test(power = .25, k=4, f=0.2, sig.level=.05)

# what's the ball park of the effect size we need to be looking at?
pwr.anova.test(power = .25, k=4, n=12, sig.level=.05)

#### Logistic Regression Model ####

install.packages('ISLR')
library(ISLR)

# We are going to see if we can predict the direction of the movement of the stock market from its previous close values. Is it possible to predict a given day's close based on whether the market has closed up or down based on the previous N days?

data("Smarket")
stock_market <- Smarket

sm.model <- glm(data = stock_market, Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial('logit'))

summary(sm.model)

# W can use our glm model to predict. First we fit the model again (this is the same).
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family=binomial, data=stock_market)

# Then we can use the predict function to see how well our model predicts any given day. 
glm.probs <- predict(glm.fit, type='response')

# Print out the first few values
head(glm.probs)

# Plot
plot(glm.probs)
