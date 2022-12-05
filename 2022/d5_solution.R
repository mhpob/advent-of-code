ship <- readLines('2022/d5_input.txt',
                  n = 8)
ship <- gsub('    ', ' []', ship)
ship <- strsplit(ship, '\\] \\[')
ship <- unlist(ship)
ship <- gsub('\\[|\\]', '', ship)
ship[ship == ''] <- NA

ship <- matrix(ship, ncol = 9, byrow = T)
ship <- split(t(ship), 1:9)

# ORiginally was going to try to fill in columns in a matrix, but we'll never
#   know how tall they need to be. So, working with vectors in a list since they
#   can all be different lengths
ship <-  lapply(ship, function(.) .[!is.na(.)])


crane <- read.table('2022/d5_input.txt',
                    skip = 10)
names(crane)[c(2, 4, 6)] <- c('move', 'from', 'to') 
crane <- crane[, c(2, 4, 6)]

for(i in 1:nrow(crane)){
  
  ship[[crane$to[i]]] <- 
    c(ship[[crane$from[i]]][seq(crane$move[i], 1)],
      ship[[crane$to[i]]])
  
  ship[[crane$from[i]]] <- 
    ship[[crane$from[i]]][-seq(1, crane$move[i])]
}

paste(lapply(ship, '[[', 1), collapse = '')


# Part 2 ----
# This is all the same, except instead of counting the sequence down
#   (seq(move, 1)) we count up seq(1, move)
ship <- readLines('2022/d5_input.txt',
                  n = 8)
ship <- gsub('    ', ' []', ship)
ship <- strsplit(ship, '\\] \\[')
ship <- unlist(ship)
ship <- gsub('\\[|\\]', '', ship)
ship[ship == ''] <- NA

ship <- matrix(ship, ncol = 9, byrow = T)
ship <- split(t(ship), 1:9)
ship <-  lapply(ship, function(.) .[!is.na(.)])


crane <- read.table('2022/d5_input.txt',
                    skip = 10)
names(crane)[c(2, 4, 6)] <- c('move', 'from', 'to') 
crane <- crane[, c(2, 4, 6)]

for(i in 1:nrow(crane)){
  
  ship[[crane$to[i]]] <- 
    c(ship[[crane$from[i]]][seq(1, crane$move[i])],
      ship[[crane$to[i]]])
  
  ship[[crane$from[i]]] <- 
    ship[[crane$from[i]]][-seq(1, crane$move[i])]
}

paste(lapply(ship, '[[', 1), collapse = '')
