# The Elf would first like to know which games would have been possible if the bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes?

d2 <- readLines('2023/d2_data.txt') |> 
  gsub(':', ';', x = _) |> 
  read.table(text = _, sep = ';', fill = T, header = F)

d2$V1 <- gsub('Game ', '', d2$V1)

d2 <- reshape(d2, idvar = 'V1', 
        direction = 'long',
        varying = list(2:ncol(d2)),
        timevar = 'draw')
d2 <- d2[d2$V2 != '',]

d2$n_red <- ifelse(
  grepl('red', d2$V2),
  as.numeric(gsub('.* (\\d*) red.*', '\\1', d2$V2)),
  0
)
d2$n_green <- ifelse(
  grepl('green', d2$V2),
  as.numeric(gsub('.* (\\d*) green.*', '\\1', d2$V2)),
  0 
)
d2$n_blue <- ifelse(
  grepl('blue', d2$V2),
  as.numeric(gsub('.* (\\d*) blue.*', '\\1', d2$V2)),
  0
)


d2$valid <- ifelse(
  d2$n_red <= 12 & d2$n_green <= 13 & d2$n_blue <= 14,
  T,
  F
)

d2_valid <- d2[!d2$V1 %in% d2[d2$valid == FALSE,]$V1,]
sum(as.numeric(unique(d2_valid$V1)))


## Part 2
d2_split <- split(d2, d2$V1)
lapply(d2_split, function(.) prod(apply(.[,4:6],2, max))) |> 
  unlist() |> 
  sum()
