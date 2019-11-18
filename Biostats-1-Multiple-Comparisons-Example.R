rm(list = ls())

set.seed(32)

# create thresholds / conditions for one observer
cond_1 <- c('A','B','C','D')
cond_2 <- c('0','6','12')
n_groups <- 2 


make_data_sim <- function(cond_1, cond_2, n_sub) {
  for (n_sub_ctr in n_sub) {
    for (cond_1_ctr in 1:length(cond_1)) {
      for (cond_2_ctr in 1:length(cond_2)) {
        if (!exists('d')) { 
          d <- c(n_sub_ctr, cond_1[cond_1_ctr], cond_2[cond_2_ctr], rnorm(1, mean = .2, sd = .01))
        } else {
          d <- rbind(d, c(n_sub_ctr,cond_1[cond_1_ctr], cond_2[cond_2_ctr], rnorm(1, mean = .2, sd = .01)))
       }
      }
    }
  }
  rownames(d) <- c()
  d <- data.frame(d)
  return(d)
}

n_sub <- 1:12
d_em <- make_data_sim(cond_1, cond_2, n_sub)
colnames(d_em) <- c('subject','blur','ecc','thresh')

re_var <- rep('Em', nrow(d_em))
d_em$re <- re_var

n_sub <- 13:24
d_my <- make_data_sim(cond_1, cond_2, n_sub)
colnames(d_my) <- c('subject','blur','ecc','thresh')

re_var <- rep('My', nrow(d_my))
d_my$re <- re_var

d <- rbind(d_em,d_my)
d$thresh <- as.numeric(d$thresh)


# totally contrived t-test example
t.test(rnorm(n=30, mean = 0.2, sd = .05), rt(n=30, df=1)+.25)

library(ez)
ezANOVA(data=d, dv=thresh, wid=subject, within=.(blur,ecc), between=re,detailed=TRUE)
