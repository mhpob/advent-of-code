library(data.table)

commands <- readLines('2022/d7_input.txt')
k <- lapply(commands, function(.) gsub('\\$ ', '', .))

k<- lapply(k, function(.)unlist(strsplit(., ' ')))

k <- k[!sapply(k, function(.) any(grepl('ls', .)))]

wd <- file.path('.')
directory <- NULL

j <- list()
for(i in seq_along(k)){
  cmd <- k[[i]]
  
  if(cmd[1] == 'cd'){
    if(cmd[2] == '..'){
      wd <- dirname(wd)
    } else{
      wd <- file.path(wd, cmd[2])
      # directory <- c(directory, wd)=
    }
    if(!wd %in% names(j)){
      j[[wd]] <- 0
    }
  }
  
  if(cmd[1] == 'dir'){
    # directory <- c(directory, file.path(wd, cmd[2]))
    dir <- file.path(wd, cmd[2])
    if(!dir %in% names(j)){
      j[[dir]] <- 0
    }
  } 
  
}


kk <- lapply(k[2:200], function(cmd){
  if(cmd[1] == '$'){
    if(cmd[2] == 'cd'){
      if(cmd[3] == '..'){
        wd <- dirname(wd)
      } else{
        wd <- file.path(wd, cmd[3])
        directory <- c(directory, wd)
      }
    }
  }
  if(cmd[1] == 'dir'){
    directory <- c(directory, file.path(wd, cmd[2]))
  } 
}
)

else{
  
}



commands <- data.table(commands)

k <- commands[grepl('^\\$', V1)]
k[, ls := fifelse(grepl('ls', V1), 1, NA)]
k[!is.na(ls), run := seq(.N, 1)]

k[, r2 := nafill(run, 'nocb')]

k <- k[!grepl('ls', V1)]

k[, V1 := gsub('\\$ cd ', '', V1)]
k[, dir := NA]
k$dir[1] <- '/'

k[, n:=.N, by = r2]
k2 <- copy(k)
k <- k[k$V1 != '..']

for(i in 2:nrow(k)){
  n <- k$n[i]
  if(k$n[i] == 0){
    k$dir[i] <- k$V1[i-1]
  } else{
    k$dir[i] <- k$V1[i-n]
  }
}

for(i in 2:nrow(k)){
 
  if(k$V1[i] != '..'){
    k$dir[i] <- k$V1[i]
  }
  
  
  
  if(k$V1[i] == '..'){
    k$dir[i] <- k$dir[i-1]
  }else{
    k$dir[i] <- k$V1[i-1]
  }
  
  if(k$dir[i] == '..'){
    k$dir[i] <- k$dir[i-1]
  }
  
}





head(commands)
key <- as.factor(commands$dir)
commands[, dir := as.numeric(as.factor(dir))]
commands[, dir := nafill(dir, 'locf')]
commands[, dir := levels(key)[dir]]
commands[, size := fifelse(grepl('\\d', type), as.numeric(type), NA)]

commands <- commands[!out %in% c('cd', 'ls'), !'type']

setnames(commands, 'out', 'name')

dir_size <- commands[, .(sum(size, na.rm = T)), by = 'dir']
setkey(dir_size, dir)
setkey(commands, name)

k <- dir_size[commands, on = c(dir = 'name')]
k[, size := fifelse(is.na(size), V1, size)]

k <- k[, .(size = sum(size, na.rm= T)), by = 'i.dir']
j<-k[size <= 100000]
# 1063479