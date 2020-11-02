#### find and plot the best linear fit ####
rm(list = ls());

library(ggplot2)
library(tidyverse)

data_x <-rnorm(1001);
data_y <- rnorm(1001);

x <- seq(-3,3,.001*6);
y <- seq(-3,3,.001*6);

ta_da <- tibble(data_x, data_y)
ta_da %>% ggplot(aes(x = data_x, y = data_y)) + geom_point() + stat_smooth(aes(x = x, y = y),method = "glm", family = "binomial")

