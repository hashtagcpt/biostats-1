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

#### massachusettes show 'em ####

# here on down -- this is just playtime

#### Bayes and Bayes Factors ####
runCardBayes <- function() {
  #rm(list = ls())
  readcolor <- function() { 
    n <- readline(prompt="Enter color of the card removed (R or B) : ")
    n <- toupper(n)
    if (is.na(n) || ((n != 'R') && (n != 'B') && (n != 'Q'))) {
      n <- readcolor()
    }
    return(n)
  }
  
  bayesrule <- function(prior, conditional1, conditional2) {
    posterior <- (prior * conditional1) / ((prior * conditional1) + (1-prior * conditional2))
  }
  
  # set the prior probability that red has been removed from the deck
  pRedRemoved <- 0.5
  pBlackRemoved <- 0.5
  
  conditionalRemoved <- 13/39 # conditional probability if a red suit has been removed
  conditionalNotRemoved <- 26/39 # conditional probability if a red suit has not been removed
  
  # set our conditional variable
  stopComputing <- FALSE
  
  while (stopComputing != TRUE) {
    card_color <- readcolor()
    if (card_color != 'Q') {
      if (card_color == 'R') {
        pRedRemoved <- bayesrule(pRedRemoved, conditionalRemoved, conditionalNotRemoved)
        pBlackRemoved <- bayesrule(pBlackRemoved, conditionalNotRemoved,conditionalRemoved)
      } else {
        pRedRemoved <- bayesrule(pRedRemoved, conditionalNotRemoved, conditionalRemoved)
        pBlackRemoved <- bayesrule(pBlackRemoved, conditionalRemoved,conditionalNotRemoved)
      }  
      bayesfactor_k <- pRedRemoved / pBlackRemoved
      print(sprintf("The probability that red was removed is %f", pRedRemoved))
      print(sprintf("The probability that black was removed is %f", pBlackRemoved)) 
      print(sprintf("Bayes Factor (K) for (H = red removed) is %f", bayesfactor_k))
    } else {
      stopComputing <- TRUE
    }
  }
  return()
}

#### below from https://www.r-bloggers.com/2014/07/the-maths-of-texas-hold-em-with-r/ ####


# A deck of 52 cards (Jokers are excluded)
deck = c("A","A","A","A","K","K","K","K","Q","Q","Q","Q","J","J","J","J",10,10,10,10,9,9,9,9,8,8,8,8,7,7,7,7,6,6,6,6,5,5,5,5,4,4,4,4,3,3,3,3,2,2,2,2)

# This function simulates a hand at Texas Hold'em poker for 2 players and the dealer
hand <- function()
{
  player1Hand <- sample(deck,2,F)
  for(i in player1Hand)
  {
    # is.element(i,deck) checks if the element i is in the vector deck
    # match gets the position of the i element in the vector deck and deck[-v] deletes the vth element of the vector deck
    if(is.element(i,deck)
    ){         
      deck<-deck[-match(i,deck)]    
    }
  }
  
  player2Hand <- sample(deck,2,F)
  for(i in player2Hand)
  {
    if(is.element(i,deck))
    {         
      deck<-deck[-match(i,deck)]
    }
  }
  
  # The flop
  dealt_cards <- sample(deck,3,F)
  for(i in dealt_cards)
  {
    if(is.element(i,deck))
    {         
      deck<-deck[-match(i,deck)]
    }
  }
  
  # The turn
  second_dealt <- sample(deck,1,F)
  for(i in second_dealt)
  {
    if(is.element(i,deck))
    {         
      deck<-deck[-match(i,deck)]
      dealt_cards = append(dealt_cards,second_dealt)
    }
  }
  
  # The river
  third_dealt <- sample(deck,1,F)
  for(i in third_dealt)
  {            
    if(is.element(i,deck))
    {         
      deck<-deck[-match(i,deck)]
      dealt_cards = append(dealt_cards,third_dealt)
    }
  }
  
  # Eventually, a list of the results is returned by the function
  results = list(player1Hand,player2Hand,dealt_cards)
  return(results)
}
