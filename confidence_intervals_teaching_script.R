# =============================================================================
#  CONFIDENCE INTERVALS FROM SCRATCH  (tidyverse version)
#  A beginner's walk-through: compute by hand, verify, then simulate what
#  "95% confidence" actually means.
#
#  How to use: run one section at a time (Ctrl/Cmd + Enter line by line, or
#  Ctrl/Cmd + Alt + T in RStudio to run the current section).
#  Requires: R >= 4.1 (for the native pipe |> and \(x) lambdas)
#            install.packages("tidyverse")   # once; includes ggplot2 + broom
# =============================================================================


# ---- 0. Setup ---------------------------------------------------------------

library(tidyverse)   # loads ggplot2, dplyr, tidyr, purrr, tibble, ...

theme_set(theme_minimal(base_size = 13))   # one clean look for every plot

# set.seed() makes the "random" numbers reproducible.
# Everyone in class who uses the same seed gets the same results.
set.seed(2026)

# In real research we NEVER know the true population values.
# Here we invent a population so we can check our work against the truth.
# Think of it as IQ-like scores: mean 100, standard deviation 15.
mu    <- 100    # TRUE population mean (the thing we are trying to estimate)
sigma <- 15     # TRUE population standard deviation
n     <- 25     # sample size
conf  <- 0.95   # confidence level
alpha <- 1 - conf   # the "error rate" we accept: 0.05

# Look at the population we are sampling from
ggplot() +
  stat_function(fun = dnorm, args = list(mean = mu, sd = sigma),
                linewidth = 1) +
  xlim(mu - 4 * sigma, mu + 4 * sigma) +
  geom_vline(xintercept = mu, linetype = "dashed", colour = "blue") +
  annotate("text", x = mu, y = 0.002, label = "mu", parse = TRUE,
           hjust = -0.5, colour = "blue", size = 6) +
  labs(title = "The population (normally unknown to us)",
       x = "Score", y = "Density")


# ---- 1. Draw ONE sample and describe it by hand ------------------------------

# A tibble is a tidyverse data frame: one row per person, one column per variable.
samp <- tibble(
  id    = 1:n,
  score = rnorm(n, mean = mu, sd = sigma)   # n random draws from the population
)
samp   # look at the raw data

# Hand calculations inside summarise():
#   sample mean = add everything up, divide by n
#   sample SD   = average squared distance from the mean, divided by (n - 1),
#                 then square-rooted. The n - 1 corrects for measuring
#                 distances from xbar instead of from the true mu.
hand <- samp |>
  summarise(
    n_obs = n(),                                         # count the rows
    xbar  = sum(score) / n_obs,                          # mean by hand
    s     = sqrt(sum((score - xbar)^2) / (n_obs - 1)),   # SD by hand
    xbar_builtin = mean(score),                          # R's versions,
    s_builtin    = sd(score)                             # for checking
  )
hand   # the hand and built-in columns should match

# Pull the two numbers out as plain values for later sections
xbar <- hand$xbar
s    <- hand$s

# Picture the sample: each dot is one person
ggplot(samp, aes(x = score, y = 0)) +
  geom_jitter(height = 0.1, width = 0, size = 2, alpha = 0.7) +
  geom_vline(xintercept = xbar, colour = "red", linewidth = 1) +
  geom_vline(xintercept = mu, colour = "blue", linetype = "dashed") +
  scale_y_continuous(NULL, breaks = NULL, limits = c(-0.5, 0.5)) +
  labs(title = "One sample: red = sample mean, blue dashed = true mean",
       x = "Score")

# KEY POINT: xbar is not equal to mu. Every sample gives a different xbar.
# A confidence interval puts a margin of error around xbar.


# ---- 2. Why the interval works: the sampling distribution of the mean -------

# Imagine repeating the study many times. Each time: draw n people,
# compute xbar. What do all those xbars look like?
n_reps <- 5000
xbars <- tibble(rep = 1:n_reps) |>
  mutate(xbar = map_dbl(rep, \(i) mean(rnorm(n, mean = mu, sd = sigma))))
# map_dbl() runs the little function once per row and returns numbers.

# Theory says: xbars are normal, centered on mu,
# with SD = sigma / sqrt(n). That SD has a special name: the STANDARD ERROR.
se_true <- sigma / sqrt(n)
z_crit  <- qnorm(1 - alpha / 2)   # qnorm(0.975) = 1.959964...

ggplot(xbars, aes(x = xbar)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 fill = "grey80", colour = "white") +
  stat_function(fun = dnorm, args = list(mean = mu, sd = se_true),
                colour = "blue", linewidth = 1) +
  geom_vline(xintercept = mu + c(-1, 1) * z_crit * se_true,
             colour = "red", linetype = "dashed") +
  labs(title = paste0("Sampling distribution of the mean (n = ", n, ")"),
       subtitle = "Blue: theory. Red lines: mu \u00b1 1.96 SE",
       x = "Sample mean", y = "Density")

# Check the theory against the simulation
xbars |>
  summarise(
    sd_of_xbars   = sd(xbar),                                   # simulated SE
    theory_se     = se_true,                                    # sigma/sqrt(n)
    prop_within   = mean(abs(xbar - mu) <= z_crit * se_true)    # ~0.95
  )

# THE CORE LOGIC OF A CONFIDENCE INTERVAL:
#   "xbar is within 1.96 SE of mu"   is the SAME statement as
#   "mu is within 1.96 SE of xbar".
# The first happens 95% of the time, so the second does too.
# So: xbar +/- 1.96 * SE catches mu in 95% of samples.


# ---- 3. Where does 1.96 come from? ------------------------------------------

# qnorm(p) returns the z value with probability p to its LEFT.
# For 95% in the middle, we leave 2.5% in each tail:
qnorm(0.975)   #  1.96
qnorm(0.025)   # -1.96

# Other confidence levels use other critical values:
levels <- c(0.80, 0.90, 0.95, 0.99)
tibble(confidence = levels) |>
  mutate(z_critical = qnorm(1 - (1 - confidence) / 2))


# ---- 4 & 5. Two versions of the CI, built by hand ----------------------------

# z-interval (sigma KNOWN):   xbar +/- z_crit * sigma / sqrt(n)
#   Rare in practice, but the simplest case.
# t-interval (sigma UNKNOWN): xbar +/- t_crit * s / sqrt(n)
#   The usual case. s is itself noisy, so we need a slightly WIDER
#   multiplier from the t distribution with n - 1 degrees of freedom.
t_crit <- qt(1 - alpha / 2, df = n - 1)

ci_table <- tibble(
  method = c("z", "t"),
  label  = c("z (sigma known)", "t (sigma unknown)"),
  se     = c(sigma / sqrt(n), s / sqrt(n)),   # standard error
  crit   = c(z_crit, t_crit)                  # critical value
) |>
  mutate(
    moe   = crit * se,       # margin of error
    lower = xbar - moe,
    upper = xbar + moe
  )
ci_table

# Verify the t-interval against R's built-in test.
# broom::tidy() turns the test output into a one-row tibble.
t.test(samp$score, conf.level = conf) |>
  broom::tidy() |>
  select(estimate, conf.low, conf.high)   # should match the "t" row above

# Plot the two intervals side by side
ggplot(ci_table, aes(y = label, xmin = lower, xmax = upper)) +
  geom_vline(xintercept = mu, colour = "blue", linetype = "dashed") +
  geom_linerange(linewidth = 1.2) +
  geom_point(aes(x = xbar), size = 3) +
  labs(title = "Our one sample, two 95% intervals",
       subtitle = "Blue dashed = true mean", x = "Score", y = NULL)

# How the t critical value shrinks toward 1.96 as n grows:
tibble(df = c(2, 4, 9, 24, 49, 99, 999)) |>
  mutate(n          = df + 1,
         t_critical = qt(0.975, df),
         z_critical = qnorm(0.975))


# ---- 6. How to read a CI (and how NOT to) ------------------------------------

# CORRECT:
#   "We are 95% confident the population mean is between L and U."
#   Meaning: the METHOD produces intervals that capture mu 95% of the time.
#
# INCORRECT:
#   "There is a 95% probability that mu is between L and U."
#     -> mu is a fixed number. This particular interval either contains it
#        or does not. The 95% describes the procedure, not this interval.
#   "95% of the data fall in the interval."
#     -> The CI is about the MEAN, not individual scores. Check:
ci_t <- filter(ci_table, method == "t")
samp |>
  summarise(prop_scores_inside = mean(between(score, ci_t$lower, ci_t$upper)))
#     (far below 0.95)
#   "95% of future sample means will fall in this interval."
#     -> Also wrong; this interval is centered on OUR xbar, not on mu.
#
# NOTE: the SAMPLE mean is always inside its own interval (it is the center).
# The interesting question is whether the POPULATION mean mu is inside.


# ---- 7. THE BIG SIMULATION: 100 studies, 100 intervals -----------------------

# Pretend 100 different labs each run the same study and each report
# a 95% t-interval. How many of them capture the true mu?
n_intervals <- 100

# Step 1: build ALL the raw data in long format:
#         one row per person, with a column saying which study they are in.
all_data <- expand_grid(study = 1:n_intervals, person = 1:n) |>
  mutate(score = rnorm(n_intervals * n, mean = mu, sd = sigma))
all_data   # 100 * 25 = 2500 rows

# Step 2: group_by(study) + summarise() = "do this separately for each lab"
sims <- all_data |>
  group_by(study) |>
  summarise(
    mean = mean(score),
    se   = sd(score) / sqrt(n()),
    .groups = "drop"
  ) |>
  mutate(
    lower  = mean - t_crit * se,
    upper  = mean + t_crit * se,
    covers = lower <= mu & upper >= mu    # TRUE if this lab caught mu
  )
sims

sims |> count(covers)   # how many hits and misses?

# Plot all 100 intervals. Misses are red.
ggplot(sims, aes(y = study, xmin = lower, xmax = upper, colour = covers)) +
  geom_vline(xintercept = mu, colour = "blue", linetype = "dashed",
             linewidth = 1) +
  geom_linerange(aes(linewidth = covers)) +
  geom_point(aes(x = mean), size = 0.8) +
  scale_colour_manual(values = c(`TRUE` = "grey40", `FALSE` = "red"),
                      labels = c(`TRUE` = "captures mu", `FALSE` = "misses mu"),
                      name = NULL) +
  scale_linewidth_manual(values = c(`TRUE` = 0.4, `FALSE` = 1.1),
                         guide = "none") +
  labs(title = paste0(n_intervals, " independent 95% CIs: ",
                      sum(sims$covers), " captured the true mean"),
       subtitle = "Blue dashed = true mean",
       x = "Score", y = "Study number") +
  theme(legend.position = "top")

# DISCUSSION: every lab thinks its interval is fine. Nobody can tell from
# their own data whether they are one of the red ones.


# ---- 8. Why didn't we get EXACTLY 95? ----------------------------------------

# The number of hits in 100 tries is itself random:
# Binomial(size = 100, prob = 0.95).
# Expected 95, but 91 to 99 is completely normal.
our_hits <- sum(sims$covers)

tibble(hits = 85:100) |>
  mutate(prob = dbinom(hits, size = 100, prob = 0.95),
         ours = hits == our_hits) |>
  ggplot(aes(x = factor(hits), y = prob, fill = ours)) +
  geom_col() +
  scale_fill_manual(values = c(`FALSE` = "grey70", `TRUE` = "red"),
                    guide = "none") +
  labs(title = "What to expect from 100 intervals (red = what we got)",
       x = "Number of intervals capturing mu (out of 100)",
       y = "Probability")

# Chance of 93 or more hits:
sum(dbinom(93:100, size = 100, prob = 0.95))

# Run MANY more intervals and the proportion settles near 0.95 (next section).


# ---- 9. Coverage in the long run, and why we need t --------------------------

# A reusable function: simulate `reps` studies of size n and report the
# proportion of CIs that capture mu, for three methods:
#   z_known  : xbar +/- 1.96   * sigma / sqrt(n)  (sigma truly known)
#   z_plugin : xbar +/- 1.96   * s / sqrt(n)      (common beginner mistake)
#   t        : xbar +/- t_crit * s / sqrt(n)      (the correct approach)
# Shortcut used below: "lower <= mu <= upper" is the same as
# "|xbar - mu| <= margin of error" (the Section 2 logic again).
coverage_sim <- function(n, reps = 10000, conf = 0.95, mu = 100, sigma = 15) {
  alpha  <- 1 - conf
  z_crit <- qnorm(1 - alpha / 2)
  t_crit <- qt(1 - alpha / 2, df = n - 1)
  
  # Same long-format idea as Section 7, just many more studies
  per_study <- expand_grid(rep = 1:reps, person = 1:n) |>
    mutate(score = rnorm(reps * n, mean = mu, sd = sigma)) |>
    group_by(rep) |>
    summarise(m = mean(score), s = sd(score), .groups = "drop")
  
  per_study |>
    summarise(
      n        = n,
      z_known  = mean(abs(m - mu) <= z_crit * sigma / sqrt(n)),
      z_plugin = mean(abs(m - mu) <= z_crit * s / sqrt(n)),
      t        = mean(abs(m - mu) <= t_crit * s / sqrt(n))
    )
}

# Long-run coverage with our n = 25 (all three should be close to 0.95)
coverage_sim(n = 25)

# Compare methods across sample sizes (takes a few seconds)
ns <- c(3, 5, 10, 20, 30, 50, 100)
cov_table <- map(ns, coverage_sim) |> list_rbind()
cov_table

# Reshape to long format so ggplot can colour by method
cov_table |>
  pivot_longer(-n, names_to = "method", values_to = "coverage") |>
  mutate(method = factor(method,
                         levels = c("z_known", "z_plugin", "t"),
                         labels = c("z, sigma known", "z with s (wrong)",
                                    "t with s"))) |>
  ggplot(aes(x = n, y = coverage, colour = method)) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_log10(breaks = ns) +
  scale_colour_manual(values = c("blue", "red", "darkgreen"), name = NULL) +
  coord_cartesian(ylim = c(0.80, 1)) +
  labs(title = "Plugging s into a z-interval under-covers at small n",
       x = "Sample size (log scale)",
       y = "Proportion of CIs capturing mu") +
  theme(legend.position = "bottom")

# TAKEAWAY: when sigma is estimated, the z-interval is too narrow for small n.
# The t distribution fixes this. By n ~ 30+ the difference is small.


# ---- 10. What controls the WIDTH of an interval? -----------------------------

# Width = 2 * critical value * s / sqrt(n)
# Three levers: confidence level, sample size, variability.

# (a) Higher confidence -> wider interval (same data, different levels)
tibble(confidence = levels) |>
  mutate(width = map_dbl(confidence,
                         \(cl) diff(t.test(samp$score, conf.level = cl)$conf.int)))

# (b) Larger n -> narrower interval. Width shrinks with sqrt(n),
#     so 4x the data gives only half the width.
tibble(n = c(10, 25, 50, 100, 200, 400)) |>
  mutate(width = 2 * qt(0.975, df = n - 1) * sigma / sqrt(n)) |>
  ggplot(aes(x = n, y = width)) +
  geom_line() +
  geom_point(size = 2) +
  labs(title = "Diminishing returns: width shrinks with sqrt(n)",
       x = "Sample size", y = "Approximate 95% CI width")

# (c) More variable population -> wider interval (try sigma = 30 in Section 0)


# ---- 11. Bridge to next lecture: CIs and hypothesis tests --------------------

# Suppose someone claims the population mean is 105.
# If 105 is OUTSIDE our 95% CI, a two-sided t-test at alpha = 0.05
# will reject that claim. If INSIDE, it will not. Same information,
# two presentations.
mu0 <- 105

tibble(
  mu0         = mu0,
  inside_ci   = between(mu0, ci_t$lower, ci_t$upper),
  t_stat      = (xbar - mu0) / ci_t$se,        # preview: the test statistic
  beyond_crit = abs(t_stat) > t_crit           # reject?
)
# These always agree:  inside the CI  <=>  |t| <= t_crit  <=>  do not reject.

# Sneak peek at next lecture's tool, in tidy form:
t.test(samp$score, mu = mu0) |> broom::tidy()


# ---- 12. Exercises -----------------------------------------------------------

# 1. Change n to 5 in Section 0 and rerun Sections 1-7. What happens to the
#    interval widths? Does coverage still hold near 95%?
# 2. Change conf to 0.80 and rerun Section 7. About how many red intervals
#    do you expect? Count them with count(covers).
# 3. In coverage_sim(), replace rnorm(...) with rexp(reps * n, rate = 1 / mu)
#    (a skewed population whose mean is still mu). Check coverage at
#    n = 5, 15, 50. When can you start trusting the t-interval?
# 4. Without running code: to halve the width of an interval, roughly how
#    much must n increase? Check your answer with Section 10(b).
# 5. Explain in one sentence, to a friend, why "95% chance mu is in THIS
#    interval" is not quite right.