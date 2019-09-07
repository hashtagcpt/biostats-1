#### Biostats 1 R Commands #### 

# all the libraries we use will go up here, we'll get to them in time
library(ggplot2)

#### the standard error ####
set.seed(999)
N <- 5000
x <- rnorm(N, mean = 0, sd = 1)
xbar <- sum(x)/length(x)
sigma <- sqrt(sum((x-xbar)^2)/length(x))
sigma_pop <- sigma / sqrt(length(x))
(sigma - sigma_pop) # will converge to 1 because that's the true population standard deviation 

#### the normality assumption ####
set.seed(100) # set the random number generator
d <- rnorm(1000)

d_dev <- (d - mean(d))
x_axis <- c(1:length(d))
dev_df <- data.frame(cbind(x_axis,d_dev))

# initialize the plot, ask for the point geom, ask for a line of intercept/slope
dev_plot <- ggplot(data = dev_df, aes(x = x_axis, y = d_dev)) + geom_point(size = 4) + geom_abline(slope = 0, intercept = mean(dev_df$d_dev), color = 'red', size = 2) 

# set the plot labels
dev_plot <- dev_plot + xlab('sample') + ylab('deviation from the mean') 

# change the them to have bigger text
dev_plot <- dev_plot + theme(text = element_text(size=20), axis.text.x = element_text(size = 16), axis.text.y = element_text(size=16))

# put the plot in the plot window tab
dev_plot

# show the deviations as a histogram
hist_plot <- ggplot(data = dev_df, aes(d_dev)) + geom_histogram() + xlab('deviation from the mean') + theme(text = element_text(size=20), axis.text.x = element_text(size = 16), axis.text.y = element_text(size=16))

hist_plot
