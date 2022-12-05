
moves <- readLines('2015/day 3/day3_input.txt')

moves <- data.frame(
  moves = 
    unlist(
      strsplit(moves, '')
    )
)

moves$x <- NA
moves$y <- NA

moves <- rbind(
  data.frame(moves = 'start',
             x = 0,
             y = 0),
  moves
)

for(i in 2:nrow(moves)){
  moves$x[i] <- ifelse(moves$moves[i] == '>', moves$x[i - 1] + 1,
                       ifelse(moves$moves[i] == '<', moves$x[i - 1] - 1,
                              moves$x[i - 1]))
  
  moves$y[i] <- ifelse(moves$moves[i] == '^', moves$y[i - 1] + 1,
                       ifelse(moves$moves[i] == 'v', moves$y[i - 1] - 1,
                              moves$y[i - 1]))
}

nrow(unique(moves[, 2:3]))

## Part 2 ----

santa <- moves[c(1, seq(2, nrow(moves), 2)),]
robo_santa <- moves[c(1, seq(3, nrow(moves), 2)),]

for(i in 2:nrow(santa)){
  santa$x[i] <- ifelse(santa$moves[i] == '>', santa$x[i - 1] + 1,
                       ifelse(santa$moves[i] == '<', santa$x[i - 1] - 1,
                              santa$x[i - 1]))
  
  santa$y[i] <- ifelse(santa$moves[i] == '^', santa$y[i - 1] + 1,
                       ifelse(santa$moves[i] == 'v', santa$y[i - 1] - 1,
                              santa$y[i - 1]))
}

for(i in 2:nrow(robo_santa)){
  robo_santa$x[i] <- ifelse(robo_santa$moves[i] == '>',
                            robo_santa$x[i - 1] + 1,
                            ifelse(robo_santa$moves[i] == '<',
                                   robo_santa$x[i - 1] - 1,
                                   robo_santa$x[i - 1]))
  
  robo_santa$y[i] <- ifelse(robo_santa$moves[i] == '^',
                            robo_santa$y[i - 1] + 1,
                            ifelse(robo_santa$moves[i] == 'v',
                                   robo_santa$y[i - 1] - 1,
                                   robo_santa$y[i - 1]))
}

moves <- rbind(santa, robo_santa)
nrow(unique(moves[, 2:3]))
