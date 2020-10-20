rm(list = ls()) # everything must go!

set.seed(222)

#This is a simulation that is kind of, but not at all, like the game 21 or Blackjack.
suits <- c('Hearts','Diamonds','Spades','Clubs') # we're not going to use this, but we could...
cards <- c(1,2,3,4,5,6,7,8,9,10,10,10,10,10,11) # let's use this! 

# one of the 4 decks does not have a joker and we know it is a black deck...
joker <- 3/4 - 1/2
#...but what aspect(s) of the problem are we missing?

dealer <- function(deck) {
  cardUp <- sample(cards)  
  cardDown <- sample(cards)
  deal <- sum(cardUp[1] + cardDown[1])
  return(deal)
}

hit <- function(deal) {
  newCard <- sample(cards)
  totalScore <- deal + newCard[1] 
  if (totalScore > 21) {
    bust <- TRUE
  } else {
    bust <- FALSE
  }
  buster <- c(bust, totalScore)
  return(buster)
}

blackjackData <- data.frame()
howManyCards <- 0
bustCtr <- 1
bustScore <- 0
hitCtr <- 1
bust <- FALSE

nHands <- 10000

for (tmp in 1:nHands) {
  score <- dealer(deck)
    buster <- hit(score)
  while (!buster[1]) {
    buster <- hit(buster[2])
    hitCtr <- hitCtr + 1
    bustCtr <- bustCtr + 1
  } 
  bustScore <- buster[2]
  # HEY LOOK -- it is the t-Distribution! With rt()...
  if (rt(1,1) > 6) {
    joker <- 1
  } else {
    joker <- 0
  }
  blackjackData <- rbind(blackjackData, c(bustScore, bustCtr, joker))
  bustCtr <- 1
}
colnames(blackjackData) <- c('score','hits','significant_joker')

mean(blackjackData$score)
mean(blackjackData$hits)
mean(blackjackData$significant_joker)