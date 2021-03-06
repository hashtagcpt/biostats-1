#### Infected! ####
rm(list = ls()) # removes all 

# The R version... 

# run this function to load it into memory
doTheTest <- function(which_die, player_result, how_many_digits, cut_off) {
  #set.seed(which_die)
  
  # do the "random" sampling -- NB it is not *really* random
  d4 <- sample(1:4)
  d6 <- sample(1:6)
  d8 <- sample(1:8)
  d10 <- sample(1:10)
  d12 <- sample(1:12)
  d20 <- sample(1:20)
  d100 <- sample(c(10,20,30,40,50,60,70,80,90,100))
  
  score <- player_result / (player_result + sum(d4[1:how_many_digits]/4, d6[1:how_many_digits]/6, d8[1:how_many_digits]/8, d10[1:how_many_digits]/10, d12[1:how_many_digits]/12, d20[1:how_many_digits]/20, d100[1:how_many_digits]/100)) 
  return(score)
}

# Rules:

# 1. run first
which_die <- readline(prompt = "Which die are you rolling? ")

# 2. run second
which_die <- strtoi(which_die)
player_result <- readline(prompt="Enter the result: ")

# 3. run third
player_result <- strtoi(player_result)
name_infected <- readline(prompt = "Enter the name of the person who is possibly infected: ")

# how many rolls should I do?
how_many_rolls <- 10
how_many_digits <- 4

# A possible interpretation of the maximum score from this game might 960. This value is incorrect, but it is a reasonable answer given what we know at this point. [BONUS!] What is the maximum score? What does it depend on? 

# That said, let's doTheTest!

# dummy var
test_result <- c()

for (tmp in 1:how_many_rolls) {
  test_result[tmp] <- doTheTest(which_die, player_result, how_many_digits, cut_off)
  Sys.sleep(0.1)
  print(c("Score: ", test_result[tmp]))
}

# Our test result doesn't really mean anything at this point, it is just a score. We need a detection cut-off.
detection_cutoff <- .5

test_result <- median(test_result)
if (test_result >= detection_cutoff) {
  sprintf("%s is infected, sorry, game over. Please play again.", name_infected)
} else {
  print("Clear to go!")
}


# runCardBayes <- function() {
#   rm(list = ls())
#   readcolor <- function() { 
#     n <- readline(prompt="Enter color of the card removed (R or B) : ")
#     n <- toupper(n)
#     if (is.na(n) || ((n != 'R') && (n != 'B') && (n != 'Q'))) {
#       n <- readcolor()
#     }
#     return(n)
#   }
#   
#   bayesrule <- function(prior, conditional1, conditional2) {
#     posterior <- (prior * conditional1) / ((prior * conditional1) + (1-prior * conditional2))
#   }
#   
#   # set the prior probability that red has been removed from the deck
#   pRedRemoved <- 0.5
#   pBlackRemoved <- 0.5
#   
#   conditionalRemoved <- 13/39 # conditional probability if a red suit has been removed
#   conditionalNotRemoved <- 26/39 # conditional probability if a red suit has not been removed
#   
#   # set our conditional variable
#   stopComputing <- FALSE
#   
#   while (stopComputing != TRUE) {
#     card_color <- readcolor()
#     if (card_color != 'Q') {
#       if (card_color == 'R') {
#         pRedRemoved <- bayesrule(pRedRemoved, conditionalRemoved, conditionalNotRemoved)
#         pBlackRemoved <- bayesrule(pBlackRemoved, conditionalNotRemoved,conditionalRemoved)
#       } else {
#         pRedRemoved <- bayesrule(pRedRemoved, conditionalNotRemoved, conditionalRemoved)
#         pBlackRemoved <- bayesrule(pBlackRemoved, conditionalRemoved,conditionalNotRemoved)
#       }  
#       bayesfactor_k <- pRedRemoved / pBlackRemoved
#       print(sprintf("The probability that red was removed is %f", pRedRemoved))
#       print(sprintf("The probability that black was removed is %f", pBlackRemoved)) 
#       print(sprintf("Bayes Factor (K) for (H = red removed) is %f", bayesfactor_k))
#     } else {
#       stopComputing <- TRUE 
#     }
#   }
#   return()
# }