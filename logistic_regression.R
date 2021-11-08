#install.packages('ISLR')
library(ISLR)
data(Smarket)

glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family=binomial(link=logit), data=Smarket)

glm.probs <- predict(glm.fit,type='response')
glm.probs[1:10]

glm.pred <- rep('Down',1250)
glm.pred[glm.probs > 0.5]  = 'Up'

table(glm.pred,Smarket$Direction)

(507+145)/1250
mean(glm.pred == Smarket$Direction)

