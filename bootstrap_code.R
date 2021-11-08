library(boot)

bootReg <- function(formula, data, ind) {
  d <- data[ind,]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

album2 <- read.delim("http://www.discoveringstatistics.com/docs/Album%20Sales%202.dat", header = TRUE)
bootResults<-boot(statistic = bootReg, formula = sales ~ adverts + airplay + attract, data = album2, R = 2000)
boot.ci(bootResults, type = "bca", index = 1)
boot.ci(bootResults, type = "bca", index = 2)
