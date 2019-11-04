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

# For the ANOVA things get a little bit more complicated, but we can use the function etaSquared from the lsr package. This function works on an ANOVA object like we saw in previous class. Let's load in genotype from MASS package and perform a two-way ANOVA.

library(MASS)
data(genotype)

# assign the genotype to a data frame for convenience
dat <- genotype

# get our anova model
anova.genotype <- aov(data = dat, Wt ~ Litter + Mother + Litter:Mother)

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

#### Categorical Variables ####

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