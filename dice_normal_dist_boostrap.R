#### normal distribution - example with dice ####

# Get the mean for the dice
# d4, d6, d8, d10, d12, d20

d4 <- 1:4
d6 <- 1:6
d8 <- 1:8
d10 <- 1:10
d12 <- 1:12
d20 <- 1:20

# Create probability vectors for each die
d4_prob <- c(rep(1 / length(d4), length(d4)), rep(0, length(d20) - length(d4)))
d6_prob <- c(rep(1 / length(d6), length(d6)), rep(0, length(d20) - length(d6)))
d8_prob <- c(rep(1 / length(d8), length(d8)), rep(0, length(d20) - length(d8)))
d10_prob <- c(rep(1 / length(d10), length(d10)), rep(0, length(d20) - length(d10)))
d12_prob <- c(rep(1 / length(d12), length(d12)), rep(0, length(d20) - length(d12)))

d20_prob <- rep(1 / length(d20), length(d20))

# list of dice values
dice_vals <- c(d4,d6,d8,d10,d12,d20)

# calculate the dice mean and sd
dice_mean <- mean(c(d4,d8,d10,d12,d20))
dice_sd <- sd(c(d4,d8,d10,d12,d20))

# need to get some data from some rolls here...
# two groups roll your dice and record the total in a google sheet

# shapiro-wilk test -- for single samples
shapiro.test(data)

# kolmogorov-smirnov
ks.test(data)

#### bootstrap card simulation ####

# the boostrap by hand
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

#### R bootstrap example ####
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

# shuffle them both
samp_1_set <- shuffle_set[1:18]
samp_2_set <- shuffle_set[19:36]

# boot function
boot_card_1 <- boot(data = samp_1_set, statistic = b.mean, R = 999)

# you can plot it
plot(boot_card_1)

# you can get a summary
boot_card_1

# get bootstrap confidence intervals
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
