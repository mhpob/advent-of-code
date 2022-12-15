forest <- readLines('2022/d8_input.txt')

forest <- unlist(strsplit(forest, ''))
forest <- matrix(as.numeric(forest), nrow = 99, byrow = T)




vis <- matrix(nrow = nrow(forest), ncol = ncol(forest))

# apply a for loop by col
v_row <- t(apply(forest, 1,
      function(.) {
        forest_row <- rep(NA, 99)
        for(i in 1:length(.)){
          forest_row[i] <- any(
            which.max(.[1:i]) == i,
            which.max(.[99:i]) == 99-(i-1)
          )
          
        }
        forest_row
      })
)

v_col <- apply(forest, 2,
      function(.) {
        forest_col <- rep(NA, 99)
        for(i in 1:length(.)){
          forest_col[i] <- any(
            which.max(.[1:i]) == i,
            which.max(.[99:i]) == 99-(i-1)
          )
          
        }
        forest_col
      })

k <- array(c(v_row, v_col), c(99, 99, 2))

for(i in 1:nrow(forest)){
  for(j in 1:ncol(forest)){
    vis[i,j] <- any(k[i,j,])
  }
}
sum(vis)
#1662

#Pt 2----
# 4 dim matrix, multiply across

up <- apply(forest, 2, function(.){
  v_up <- rep(NA, length(.))
  for(i in seq_along(.)){
    if(i == 1){
      v_up[i] <- 0
    }else{
      taller <- .[(i-1):1] < .[i]
      if(all(taller)){
        v_up[i] <- length(taller)
      }else{
        v_up[i] <- which.min(taller)
      }
      
    }
  }
  v_up
}
)

down <- apply(forest, 2, function(.){
  v_down <- rep(NA, length(.))
  for(i in seq_along(.)){
    if(i == length(.)){
      v_down[i] <- 0
    }else{
      taller <- .[(i+1):nrow(forest)] < .[i]
      if(all(taller)){
        v_down[i] <- length(taller)
      }else{
        v_down[i] <- which.min(taller)
      }
    }
  }
  v_down
}
)

left <- apply(forest, 1, function(.){
  v_left <- rep(NA, length(.))
  for(i in seq_along(.)){
    if(i == 1){
      v_left[i] <- 0
    }else{
      taller <- .[(i-1):1] < .[i]
      if(all(taller)){
        v_left[i] <- length(taller)
      }else{
        v_left[i] <- which.min(taller)
      }
      
    }
  }
  v_left
}
)

left <- t(left)

right <- apply(forest, 1, function(.){
  v_right <- rep(NA, length(.))
  for(i in seq_along(.)){
    if(i == length(.)){
      v_right[i] <- 0
    }else{
      taller <- .[(i+1):nrow(forest)] < .[i]
      if(all(taller)){
        v_right[i] <- length(taller)
      }else{
        v_right[i] <- which.min(taller)
      }
    }
  }
  v_right
}
)
right <- t(right)

k <- array(c(up, down, left, right), dim = c(nrow(forest),ncol(forest),4))
k <- aperm(k, c(3,2,1))

scores <- matrix(NA, nrow = nrow(forest), ncol = ncol(forest))
for(i in 1:nrow(forest)){
  scores[i,] <- apply(k[,,i], 2, prod)
}
max(scores)
# 537600