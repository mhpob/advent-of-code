lights <- read.delim('2015/d6_input.txt',
                     header = F,
                     col.names = 'inst')

lights <- within(lights, {
  what = gsub(' \\d.*', '', inst)
  start = regmatches(inst, regexpr('\\d{1,},\\d{1,}', inst))
  end = gsub('.* ', '', inst)
  #extract coords, plus one since R is 1-indexed
  start_x = as.numeric(gsub(',.*', '', start)) + 1
  start_y = as.numeric(gsub('.*,', '', start)) + 1
  end_x = as.numeric(gsub(',.*', '', end)) + 1
  end_y = as.numeric(gsub('.*,', '', end)) + 1
})

show <- matrix(FALSE, nrow = 1000, ncol = 1000)

for(i in 1:nrow(lights)){
  if(lights$what[i] == 'turn off'){
  show[seq(lights$start_x[i], lights$end_x[i]),
       seq(lights$start_y[i], lights$end_y[i])] <- FALSE
  } else if(lights$what[i] == 'turn on'){
    show[seq(lights$start_x[i], lights$end_x[i]),
         seq(lights$start_y[i], lights$end_y[i])] <- TRUE 
  } else{
    show[seq(lights$start_x[i], lights$end_x[i]),
         seq(lights$start_y[i], lights$end_y[i])] <-
      !show[seq(lights$start_x[i], lights$end_x[i]),
            seq(lights$start_y[i], lights$end_y[i])]
  }
    
}

sum(show)
## Pt 2----
show <- matrix(0, nrow = 1000, ncol = 1000)

for(i in 1:nrow(lights)){
  if(lights$what[i] == 'turn off'){
    show[seq(lights$start_x[i], lights$end_x[i]),
         seq(lights$start_y[i], lights$end_y[i])] <- 
      show[seq(lights$start_x[i], lights$end_x[i]),
           seq(lights$start_y[i], lights$end_y[i])] - 1
  } else if(lights$what[i] == 'turn on'){
    show[seq(lights$start_x[i], lights$end_x[i]),
         seq(lights$start_y[i], lights$end_y[i])] <- 
      show[seq(lights$start_x[i], lights$end_x[i]),
           seq(lights$start_y[i], lights$end_y[i])] + 1 
  } else{
    show[seq(lights$start_x[i], lights$end_x[i]),
         seq(lights$start_y[i], lights$end_y[i])] <-
      show[seq(lights$start_x[i], lights$end_x[i]),
            seq(lights$start_y[i], lights$end_y[i])] + 2
  }
  
  show[show < 0] <- 0
  
}

sum(show)


ss <- show[1:10, 1:10]
ss[1,2] <- ss[1,2] - 1
ss[1,1] <- ss[1,1] + 1
ss[2,1] <- ss[2,1] + 2
