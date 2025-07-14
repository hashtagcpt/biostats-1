rm(list = ls())

library(tidyverse)
library(lestat)
library(broom)
library(MASS)

N <- 50
s <- 1000

set.seed(42)

sim_data <- tibble(study = rep(1:s, each=N), x = rnorm(N * s), y = rnorm(N * s))
sim_data

rho <- 0.33 #set the correlation level
x<-rnorm(N*s) 
y<-x*rho+sqrt(1-rho^2)*rnorm(N*s) #this generates the correlated parts of X and Y
sim_data <- tibble(study = rep(1:s, each=N), x = x, y = y)
sim_data


sim_data %>% dplyr::select(x,y) %>% cor()

sim_data %>% dplyr::summarise(
  count = dplyr::n(),
  mean.x = mean(x), sd.x = sd(x),
  mean.y = mean(y), sd.y = sd(y)
)

sim_data %>% group_by(study) %>%
  dplyr::summarise(
    count = dplyr::n(),
    mean.x = mean(x), sd.x = sd(x),
    mean.y = mean(y), sd.y = sd(y)
  )

res <- sim_data %>%
  group_by(study) %>%
  dplyr::summarize(r = cor(x, y)) %>%
  arrange(desc(r))

res

res %>% dplyr::summarise(average.cor = mean(r))

sim_data %>% dplyr::summarise(global.cor = cor(x,y))

sim_data %>% slice_sample(n = 10000) %>%
  ggplot(aes(x, y)) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "lm", se = FALSE)

res %>%
  ggplot(aes(x = r)) +
  geom_histogram(aes(y = stat(density)), alpha = 0.5) +
  geom_density(color = "blue", lwd = 1)

sim_data %>% filter(study %in% 1:10) %>%
  mutate(study = as_factor(study)) %>%
  ggplot(aes(x, y, color = study)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE)

sim_data %>% filter(study == res$study[which.max(res$r)]) %>%
  ggplot(aes(x, y)) +
  geom_point(size = 3, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE)

pvalues <- sim_data %>% group_by(study) %>% dplyr::summarize(tidy(lm(y ~ x))[2,5]) %>% arrange(p.value)

res <- merge(res, pvalues, by = "study")

co_r <- 0.1
co_p <- 0.05

res2 <- res %>% mutate(Outcome = case_when(
abs(r) > co_r & p.value < co_p ~ "Substanive and significant", abs(r) > co_r & p.value >= co_p ~ "Substanive but not significant", abs(r) <= co_r & p.value < co_p ~ "Not substanive but significant", abs(r) <= co_r & p.value >= co_p ~ "Not substanive and not significant", TRUE ~ "Other"))
              
res2 %>% filter(r >= 0) %>% ggplot(aes(r, p.value, color = Outcome)) + geom_point(alpha=0.5) +
geom_hline(yintercept = co_p, lty = 2, alpha = 0.5) +
geom_vline(xintercept = co_r, lty = 2, alpha = 0.5)
              
res2 %>% dplyr::select(Outcome) %>% table() / s