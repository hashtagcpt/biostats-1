#### hide_and_seek.R ####
set.seed(660)

grid <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)

whereShip <- sample(grid)
whereShip <- whereShip[1]

giveUp <- TRUE

nTries <- 13

for (timp in range(1, nTries)) {
  which_die <- readline(prompt = "Which square are you aiming to hit? ")
  if (numeric(which_die) == numeric(whereShip)) {
    did_i_hit <- rnorm(1)
    print(did_i_hit)
    hit_accept <- readline(prompt = "Do you accept this HIT? (Y/N) ")
    if (hit_accept == 'Y') {
      print('Sorry.')
      giveUp <- FALSE
    } else {
      print('Keep on keeping on.')
    }
  }   
} 
