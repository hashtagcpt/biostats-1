runCardBayes <- function() {
  rm(list = ls())

  readcolor <- function() {
    repeat {
      n <- toupper(readline(prompt = "Enter color of the card drawn (R or B), or Q to quit: "))
      if (n %in% c("R", "B", "Q")) return(n)
      cat("Invalid input. Please enter R, B, or Q.\n")
    }
  }

  bayesrule <- function(prior, conditional1, conditional2) {
    posterior <- (prior * conditional1) / ((prior * conditional1) + ((1 - prior) * conditional2))
    return(posterior)
  }

  # Initial priors
  pRedRemoved <- 0.5
  pBlackRemoved <- 0.5

  # Likelihoods
  conditionalRemoved <- 13 / 39  # P(R | Red removed)
  conditionalNotRemoved <- 26 / 39  # P(R | Black removed)

  while (TRUE) {
    card_color <- readcolor()
    if (card_color == 'Q') break

    if (card_color == 'R') {
      pRedRemoved <- bayesrule(pRedRemoved, conditionalRemoved, conditionalNotRemoved)
      pBlackRemoved <- bayesrule(pBlackRemoved, conditionalNotRemoved, conditionalRemoved)
    } else {
      pRedRemoved <- bayesrule(pRedRemoved, conditionalNotRemoved, conditionalRemoved)
      pBlackRemoved <- bayesrule(pBlackRemoved, conditionalRemoved, conditionalNotRemoved)
    }

    bayesfactor_k <- pRedRemoved / pBlackRemoved

    cat(sprintf("P(Red Removed | data) = %.4f\n", pRedRemoved))
    cat(sprintf("P(Black Removed | data) = %.4f\n", pBlackRemoved))
    cat(sprintf("Bayes Factor (Red vs Black) = %.4f\n\n", bayesfactor_k))
  }

  cat("Session ended.\n")
}
