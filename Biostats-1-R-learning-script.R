#### BIOSTATS 1 - R INTRO SCRIPT ####

### WEEK 1 ####

# A very powerful command that will delete all the variables in the environment. It's for situations similar to those where you'd restart your phone because it just isn't working right.
rm(list = ls()) 

#### Block Randomization in R ####

# Recall that we discussed block randomization briefly in class and why it is superior to simple randomization. Let's dig deeper into how to get a block randomized design with R.

# First we need the right tools. Let's get them.

install.packages('psych')
library(psych)

(condition_list <- block.random(n = 40, ncond = 2)) # block random needs your n to be a multiple of the conditions

library(knitr)
condition_list %>% kable() %>% kable_styling() 
 
#### R as a calculator ####

# basic operations
21 + 49

v <- 21 + 49
v1.1 <- 7*3 + 7*7 

# Silly trick: pick a number between 1 and 10 and assign it to original -- but it shows us how variables get over-written.
original <- 2
x <- original
# double it
x <- x*2
# multiply the new number by 5
x <- x*5
# divide by the original number
x <- x / original
# subtract 7 from the number
x <- x - 7

# print to the console - answer will always be 3! 
print(x)
x

# concatinate and case sensitivity
x <- c(1,2,3)
X <- c(1:10)
v <- 'a'
v1 <- 'b'
v2 <- 'c'
vVector <- c(v, v1, v2)

# function examples round()
v2 <- round(7^1.565)+ 7^2
vRoot <- sqrt(v2) 
log(v2)
log10(vRoot)
log(vRoot, base = 10)
# modulo
13 %% 4

# %>%

# getting help
?log

# See also the help tab in the Files, Plots, Packages, Help, and Viewer tab.
# Also many of the basic recipies for much of what you'll want to do can be found at the site http://www.cookbook-r.com

#### packages ####

# installing packages
install.packages('ggplot2') # only need to do this once

# after it is in your environment you call
library(ggplot2)

# install the tidyverse
#install.packages('tidyverse')

# Want to update you packages to the latest versions?
update.packages()

# More on packages and libraries later, but R, like all programming languages relies on extensions to the base language. In python you import, matlab you'll have external toolboxes, in C/C++ you'll have header files. This type of programming is often referred to as modular programming. The downside? Sometimes more than half the battle is finding the right library, package, extension, software development kit, or application programmer's interface, et cetera to do the job you need. 

# Back to R. Before you run it read the code below and ask yourself the question what is the value of y after the third line?
x <- 50
y <- x * 2
x <- 80

# Beginning data frames! Dataframes are the most useful data structure (a general term for how data are arranged) in R. 

# Here is how to create a sample data frame. You'll likely never see code like this again because you'll be reading data in from files.
data <- read.table(header=T, text='
 subject sex threshold
       1   M    0.013
       2   F    0.021
       3   F    0.011
       4   M    0.011
 ')

# values in a data frame can be accessed via square brackets []. Let's get the threshold for the third subject.
data[3,3]

# You can also get the entire row, in this case a single subject, subject 3.
data[3,]

# or all the thresholds that are stored in column 3.
data[,3]

# but that's not the most useful way to do it, because you'd have to remember the column in which you stored the data. We can use the $ notation to access variables within in a dataframe.

data$threshold

# you can also pop out columns of a dataframe easily...

# popping out
new_data_vector <- data$threshold

# putting together
d<-data.frame(rbind(vVector,c(1,2,3)))


#### fundamental programming concepts ####

# We'll usually need to clean-up and reformat data (here I mean data in the general comp sci sense, not data from an experiment) via code. This is not unique to R. Other languages have more elaborate and specific techniques for changing one type of data to another. R is (trust me) on the user friendly end of the spectrum of computer lanagues. To do our clean-up and other tasks where we need to tell the computer to make a decision, we'll need statements that control the flow of a program. What is flow? In short, it is what code gets executed at a given step. Let's dive in...

# The if statement. 
# This is a fundamental statement most programing languages. An if statement works linke a logical statement "If X then Y". Below is an if statement in action.

test_variable <- TRUE  # NB naming variables is an art, not too long, not too short, you don't want spaces and they can't start with a number.
var_to_print <- 3
if (test_variable == TRUE) {
  print(data$threshold[var_to_print])
}

# If we break down this 'if' we have two main parts. The first is the statment enclosed in () that follows our keyword 'if'*. This is a logical statement, if it evaluates to TRUE then the part of the code in curly braces {}, the code block will be executed. Try changing the value of 'test_variable' to FALSE and re-run this code.

### *NB R has certain keywords that you can use for variable names or if you choose a variable name that is that of a function, weird things can happen..)

# With if statements we can happy test conditions and use code blocks. Often you'll see if statements chained together in this way, an 'if' statement, one or more 'else if' statements, then 'else'. Here is an example.

blue_jays_winning_percentage <- .455 #.601

if (blue_jays_winning_percentage > .600) {
  print('The Blue Jays are better than OK!')
} else if (blue_jays_winning_percentage < .600 && blue_jays_winning_percentage >= .500) {
  print('OK Blue Jays!')
} else if (blue_jays_winning_percentage < .500 && blue_jays_winning_percentage >= 0 ) {
  print('Not OK Blue Jays.')
} else {
  print('Is it winter? Go Leafs Go.')
}        

# A full list of logical operators can be found at: https://www.statmethods.net/management/operators.html
# Part of the fun and challenge of programming is figuring out tricks of logic that can make your life easier. 

# Another type of program control flow are called loops. In general think of a loop as a command that tell the computer to repeat a block of code a number of times that can be based on a pre-set number (a for loop) or based on a logical condition. Here is an example of a for loop.

for (ind in 1:length(example_data$Condition)) {
  print(example_data$OD[ind]^2)
}

# sometimes it is simple and you know the number of times you want to do things:

for (counter in 1:10) {
  print(counter)
}

# Here is an example of a while loop that does the exact same thing as our first for loop.

ctr <- 1
while (ctr <= length(example_data$Condition)) {
  print(example_data$OD[ctr]^2)
  ctr <- ctr + 1
}

# QUESTION -- when would we need to use a for loop versus a while loop?

# User defined functions. These can *really* make your life easier and code cleaner, if you find yourself repeating a lot of code in your script. For example, if you want to reproduce several of the same type of plot with different x/y variables, you can write a function that takes those x/y variables as inputs and reproduce the plot. Here is a contrived example of a user-defined function in R

funny_add <- function(var1,var2) {
  var1 + var2 + var1/2 + var2/2
}

#### R Script Commands - Estimation & Hypothesis Testing ####

# generate random sample
d <- rnorm(15,mean = 1, sd = 2)  

# basic confidence intervals known sigma
m <- mean(d)
sigma <- 2
ciLower <- m - 1.96 * sigma / sqrt(length(d)) 
ciUpper <- m + 1.96 * sigma / sqrt(length(d)) 

# confidence interval with an estimated sigma
m <- mean(d)
sigma <- sd(d)

# qt() is the analogous function to qnorm for the t distribution
tCrit <- 1.75 # from qt(.05,15)
ciLower <- m - tCrit * sigma / sqrt(length(d)) 
ciUpper <- m + tCrit * sigma / sqrt(length(d)) 

# margin of error
sigma = 2
d<-rt(100,15)
marginOfError <- 1.96* sigma / sqrt(length(d)) 

# binary random variable confidence interval estimate
p <- 0.04 # probability of disease
n <- 100 # number cases
se <- sqrt(p*(1-p)/n)

ciUpper <- 0.04 + 1.96 * se
ciUpper * populationEstimate # number of adults older than 65

# sample size estimate
criticalValue <- 2.33
sigma <- 15
moe <- 3
n <- (criticalValue*sigma/moe)^2

# qnorm and qt
qnorm(0.025) # alpha == .05
?qt # for the t dist
qt(.025,44) #alpha .05 w/44 df

# calculating probability with the t distribution
pt(1,3)-pt(-2,3)

# pnorm / z-test
pnorm(c(36.4), mean=37, sd=0.5)
# z-test uses standarized scores
zval = (37 -  36.4) / 0.5
# zval == 1.2
1-pnorm(zval)

# one-sided hypothesis test using the R-console
mS <- 32.31
mH0 <- 30
s <- 6.13 # sd(data)
n <- 200
tScore <- (mS - mH0) / (s/sqrt(n)) 
pt(tScore, df = n-1, lower.tail=FALSE)

#### One-sample t-tests - Oct-2-18 ####

# code to generate simulated data
set.seed(100)

n_subjects <- 30
sample_data <- rnorm(n = n_subjects, mean = 10, sd = 3)

# define our null hypothesis 
h_null <- 8

# calculate sample mean
sample_mean <- mean(sample_data)

# calculate standard error
standard_deviation <- sd(sample_data) / n_subjects

t <- (sample_mean - h_null) / standard_deviation / sqrt(n_subjects)  

# We can then use the pt function to calculate the probability of obtaining the data we observed or that which is more extreme. The line of code to do that would use the pt() function again, to access the t-distribution with degrees of freedom of the number of subjects minus one.

(1-pt(t, n_subjects - 1)) * 2 # this is a two-sided test 

# This is a worked example, but we would typically use a function in R to take care of all this grunt work for us...

my_ttest <- t.test(sample_data, mu = 1, alternative = 'greater')

# What does this mean? We've set our null hypothesis as the mean of the sample is less than or equal to 8. If we find a sample mean that is very different from 8 we will get a large t-statistic and consequently a small t-statistic.

#### Two-sample T-test Example - Oct-2-18 ####

# We'll reuse the sample data generated above but create a second set of data.
sample_data_1 <- sample_data
sample_data_2 <- rnorm(n = n_subjects, mean = 10.5, sd = 3)

# calculate sample mean
sample_mean_1 <- sample_mean
sample_mean_2 <- mean(sample_data_2)

# calculate standard error
standard_error <- sqrt(var(sample_data_1) / n_subjects + var(sample_data_2) / n_subjects)

t2 <- (sample_mean_1 - sample_mean_2) / standard_error
t2

# Let's compare the t-value that we obtained from using a two sampled t-test in R.

t.test(sample_data_1, sample_data_2, alternative = 'greater')

# Setting the variances equal implies an assumption that the variance of the two populations is equal. Excel t-tests make this assumption. This is an assumption that is generally incorrect. To get a corrected version, do not use the var.equal parameter.

t.test(sample_data, sample_data_2)

# Using this applies a correction to the degrees of freedom. Note the non-integer degrees of freedom.

#### Categorical Variables - Oct-2-18 ####

# expected frequency counts
expected1 <- 16
expected2 <- 24

# observed frequency counts
observed1 <- 22
observed2 <- 18

# Q statistic is a statistic that can be obtained when one has a distrubtion of expected frequency counts and makes an observation. It follows the chi-squared distribution. An intuition of what the Q statistic computes is a deviation from the observed. We are going to use the deviations (or variability) from the expected frequencies to inform our statistical test.

Q <- (observed1 - expected1)^2 / expected1 + (observed2 - expected2)^2/ expected2 

# We will select and visualize the chi-squared distribution. To visualize its shape. 

# Let's draw a 1000 random numbers from the F distribution with one degree of freedom.
chi_dist <- rchisq(n= 1000,1)

# R has some handy functions for creating quick and dirty plots. Functions such as boxplot, plot, and hist -- let's use hist() to visualize the F distribution sample we generated.

hist(chi_dist)

# To get p-values for a Q statistic we can use the pchi command. Let's look at the upper tail and the p-value associated with our value of Q.

1-pchisq(Q,1,1)

# We can also use the chi-square test. It is the function chisq.test(). We need to place our expected and observed counts into a matrix form first.

two_by_two <- cbind(c(observed1,observed2),c(expected1,expected2))
                    
# create a matrix, 2 by 2 contingency table
chisq.test(two_by_two, correct = FALSE)

# This gives the same (not-identical for simplicity) answer as our Q statistic. The chi-squared test has some flexibility. For example, let's consider the example of a multiple choice test with 5 answers and a class of 150 students. We can input the observed number of students that selected each answer and have as our expected probabilities as each answer being equally likely to be selected.

chisq.test(c(50,50,25,25,0), p = c(0.2,0.2,0.2,0.2,0.2))

# Clearly the test shows that the students were not randomly guessing between the answers. However, when writing the test the instructor believes that the first and second answers are likely to be chosen 35% of the time, the third and fourth each 14% of the time and the fifth, very rarely. Our chi-square test would then have the following for our expected probabilities.

chisq.test(c(63,65,10,10,2), p = c(.35,.35,.14,.14,.02))

# We get a warning message because the test is using an approximation because of the low probability of the fifth choice. This is OK, R is just warning us of this fact. However the significant p-value on this test shows that the instructor's expectations on the difficulty of the options on the test are not supported by the observed data. Many fewer students were fooled by options 3 and 4 than the instructor expected.

#### ANOVA - Oct-2-18 ####
rm(list = ls()) # clear your environment
library(MASS) # load the MASS package (you may need to install it)
attach(Cushings) # The MASS package contains an example dataset called Cushings
d <- Cushings

# Sometimes a data set contains implausible values. One way to check for outliers is to use R's basic box-plot function.

boxplot(d$Tetrahydrocortisone)

# If we knew, for example, that a score on the test for Tetrahydrocortisone had a maximum score of 50 we might want to remove that data point in the following way. 

# Let's create a new data variable. That is TRUE if Tetrahyrdo is greater than 50. 
d_cleaner <- !d$Tetrahydrocortisone > 50 

# We are introducing three new bits of syntax here. First, the greater than sign, returns the logical value of TRUE if Tetrahydro is greater than our maximum score of 50. However, this is precisely what we don't want. To do this we use the exclaimation mark ! -- this changes logical values of TRUE to FALSE and vice versa. 

# Now we have a vector that is TRUE for the rows with reasonable data and FALSE for those with outliers. We can use this vector to drop outliers and remove the entire row.
d_dropped <- d[d_cleaner,]

# However, we have a situation where we know that our measure maxes out at 50. If we merely want a ceiling of 50 on our data we can use the following syntax.
d_ceiling <- d
d_ceiling$Tetrahydrocortisone[d_ceiling$Tetrahydrocortisone > 50] <- 50
boxplot(d_ceiling$Tetrahydrocortisone)

# Notice that we have several levels, under 'type' in our data set. This is the type of data upon which we would do a one-way ANOVA.

# Here it is...
results <- aov(data = d, d$Tetrahydrocortisone ~ d$Type + d$madeup)
summary(results)

# the library for Tukey's Honestly Significant Difference TukeyHSD
library(agricolae)
TukeyHSD(results)

#### Loading in Data ####

# read in .csv -- easiest
example_data <- read.csv('Example-Data-1.csv')

# read in an excel xlsx file
install.packages('readxl')
library(readxl)

# read in excel -- must supply the sheet name or index
example_excel <- read_xlsx('Example-Data-1.xlsx',sheet = 1)

# read in a matlab .mat file

install.packages('R.matlab')
library('R.matlab')

example_matlab <- readMat('CPT_1_16_blurLevelDiscrim.mat')
head(example_matlab)
example_data$OD
OD <- example_data$OD

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
sample_data_1 <- rnorm(n = n_subjects_1, mean = 15.5, sd = 1.5)
sample_data_2 <- rnorm(n = n_subjects_2, mean = 10.5, sd = 1.5)

example.t <- t.test(sample_data_1, sample_data_2)
example.t

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

#### Regression Analysis ####
dd<-read.delim('saltBP.txt', sep = ' ')

# use the linear model function
saltlm <- lm(data = dd, BP ~ saltLevel)

summary(saltlm)

# load in birthwt data from the MASS package
library(MASS)
data("birthwt")
dbirthwt <- birthwt

# perform the regression analysis using lm()

bw_reg <- lm(data = dbirthwt, bwt ~ age + smoke)
summary(bw_reg)

bw_reg_full <- lm(data = dbirthwt, bwt ~ age + smoke + age * smoke)
summary(bw_reg_full)

bw_reg_full <- lm(data = dbirthwt, bwt ~ smoke + race+ age)
summary(bw_reg_full)

#### Logistic regression #### 
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

#### the bootstrap ####

#bootstrap by hand example

my_sample <- c(6,4,8,3,8,4,2,9,7,5,6,2,5,3,6,7,10,5)

sample_mean <- mean(my_sample)

sample_sd <- sd(my_sample)

# edit these for the bootstrap game
sample1 <- mean(c(10,8,7,9,8))
sample2 <- mean(c(8,5,10,7,5))
sample3 <- mean(c(2,3,6,9,6))
sample4 <- mean(c(4,8,6,2,6))
sample5 <- mean(c(5,5,4,9,5))

boot_mean <-mean(c(sample1,sample2,sample3,sample4,sample5))

#### bootstrap full simulation ####

library(boot)

# bootstrap CI

b.mean <- function(d,i) {
  mean_boot <- mean(d[i])
  return(mean_boot)
}

# cards
full_set <- c(rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,4),rep(7,4),rep(8,4),rep(9,4),rep(10,4))

# take half the deck
shuffle_set <- sample(full_set)
samp_1_set <- shuffle_set[1:18]
samp_2_set <- shuffle_set[19:36]

# boot function
boot_card_1 <- boot(data = samp_1_set, statistic = b.mean, R = 999)

# bootstrap confidence intervals
boot_card_1_ci <- boot.ci(boot.out = boot_card_1, type = c("perc"), index = 1)
boot_card_1_ci

boot_card_2 <- boot(data = samp_2_set, statistic = b.mean, R = 999)

# bootstrap confidence intervals
boot_card_2_ci <- boot.ci(boot.out = boot_card_2, type = c("perc"), index = 1)
boot_card_2_ci

# bootstrap on *your* sample from above
boot_my_sample <- boot(data = my_sample, statistic = b.mean, R = 999)
# plot(boot_my_sample)
boot_my_sample_ci <- boot.ci(boot.out = boot_my_sample, type = c("perc"), index = 1)

#### Bayes and Bayes Factors ####
runCardBayes <- function() {
  rm(list = ls())
  readcolor <- function() { 
    n <- readline(prompt="Enter color of the card removed (R or B) : ")
    n <- toupper(n)
    if (is.na(n) || ((n != 'R') && (n != 'B') && (n != 'Q'))) {
      n <- readcolor()
    }
    return(n)
  }
  
  bayesrule <- function(prior, conditional1, conditional2) {
    posterior <- (prior * conditional1) / ((prior * conditional1) + (1-prior * conditional2))
  }
  
  # set the prior probability that red has been removed from the deck
  pRedRemoved <- 0.5
  pBlackRemoved <- 0.5
  
  conditionalRemoved <- 13/39 # conditional probability if a red suit has been removed
  conditionalNotRemoved <- 26/39 # conditional probability if a red suit has not been removed
  
  # set our conditional variable
  stopComputing <- FALSE
  
  while (stopComputing != TRUE) {
    card_color <- readcolor()
    if (card_color != 'Q') {
      if (card_color == 'R') {
        pRedRemoved <- bayesrule(pRedRemoved, conditionalRemoved, conditionalNotRemoved)
        BlackRemoved <- bayesrule(pBlackRemoved, conditionalNotRemoved,conditionalRemoved)
      } else {
        pRedRemoved <- bayesrule(pRedRemoved, conditionalNotRemoved, conditionalRemoved)
        pBlackRemoved <- bayesrule(pBlackRemoved, conditionalRemoved,conditionalNotRemoved)
      }  
      bayesfactor_k <- pRedRemoved / pBlackRemoved
      print(sprintf("The probability that red was removed is %f", pRedRemoved))
      print(sprintf("The probability that black was removed is %f", pBlackRemoved)) 
      print(sprintf("Bayes Factor (K) for (H = red removed) is %f", bayesfactor_k))
    } else {
      stopComputing <- TRUE
    }
  }
  return()
}

# Bayes Factor Package
install.packages('BayesFactor')
library('BayesFactor')

# Let's look at a simple example of computing a Bayes factor with for a linear correlation.

# We'll use the built-in R data set on the flower characteristics of the iris. Specifically the sepal (the name of the part of the flower that contains the pollen, not just a petal) width and length. 
data(iris)
plot(Sepal.Width ~ Sepal.Length, data = iris)
abline(lm(Sepal.Width ~ Sepal.Length, data = iris), col = "red")

# Let's take a standard Pearson correlation.
cor.test(y = iris$Sepal.Length, x = iris$Sepal.Width)

# We can use the BayesFactor function to determine how much evidence that we have for our hypothesis that the correlation between the width and length of the sepal is not 0.

# Note that the p-value is not significant. But do we have any evidence of a non-zero correlation?
bf <- correlationBF(y = iris$Sepal.Length, x = iris$Sepal.Width)
bf

# Some... small Bayes factors say there's some small amount. But we can look at the results of our resampling (recall: Bootstrap procedure). 

samples <- posterior(bf, iterations = 99999)
summary(samples)

# Let's check back in with what the Frequentist stats said...
cor.test(y = iris$Sepal.Length, x = iris$Sepal.Width)

plot(samples[,'rho'])

# predictions
#
guess_avg <- c(.60, .68, .69, .65, .62, .70, .8)
guess_std <- c(.15, .20, .05, .09, .07, .08, .08)

#### Multiple Comparisons ####

scores <- c(35/34, 34/34,32/34,31/34,31/34,26/34,31/34)
print(scores)
sd(scores)

#### Margin of Error and Sample size estimation #### 

sigma <- 0.5
d <- rnorm(1066)

marginOfError <- 1.96* sigma / sqrt(length(d)) 
marginOfError

# What is the n required for a specified margin of error? How do we get those polls that have a "margin of error of +/- 3%, 19 times out of 20"

sigma <- 0.5
criticalValue <- 1.96
moe <- 0.03

n <- (criticalValue*sigma/moe)^2

#### Power Analysis - Review ####

install.packages('pwr')
library(pwr)

# what is the power of a two-group t-test with a moderate effect size and an alpha of .05?
pwr.t.test(n=48*2,d=.6,sig.level=0.05)

# what is the power of a four variable moderate effect size anova?
pwr.anova.test(k=3, n=12, f=0.2, sig.level=.05)

# we can use this to calculate a required N
pwr.anova.test(power = .25, k=4, f=0.2, sig.level=.05)

# what's the ball park of the effect size we need to be looking at?
pwr.anova.test(power = .25, k=4, n=12, sig.level=.05)

