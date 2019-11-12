#### Bayes and Bayes Factors ####
runCardBayes <- function() {
  rm(list = ls())
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
        BlackRemoved <- bayesrule(pBlackRemoved, conditionalNotRemoved,conditionalRemoved)
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