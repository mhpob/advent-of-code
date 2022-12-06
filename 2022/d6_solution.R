packet <- readLines('2022/d6_input.txt')

char_diff <- 0
i <- 0

while(char_diff != 4){
  i <- i + 1
  subs <- substr(packet, i, i + 3)
  
  subs <- unlist(
    strsplit(
      subs, ''
    )
  )
  
  char_diff <- length(unique(subs))
}

i + 3


## PT 2----
char_diff <- 0
i <- 0

while(char_diff != 14){
  i <- i + 1
  subs <- substr(packet, i, i + 13)
  
  subs <- unlist(
    strsplit(
      subs, ''
    )
  )
  
  char_diff <- length(unique(subs))
}

i + 13
