
d1 <- readLines('2015/day 1/day1_input.txt')

## Pt 1 ----
up <- gsub('\\)', '', d1)
down <- gsub('\\(', '', d1)

nchar(up) - nchar(down)


## Pt 2 ----
map <- data.frame(
  path = unlist(strsplit(d1, ''))
)

map$path_decode <- ifelse(map$path == '(', 1, -1)
map$floor <- map$path_decode

for(i in 2:length(map$path)){
  map$floor[i] <- map$floor[i] + map$floor[i-1]
}

# Return first index where floor is -1
match(-1, map$floor)
