
## Part 1 ----
dims <- read.delim('2015/day 2/day2_input.txt',
                   header = F)

dims <- do.call(rbind, strsplit(dims$V1, 'x'))

dims <- as.data.frame(dims)
names(dims) <- c('l', 'w', 'h')

dims <- within(dims, {
  l <- as.numeric(l)
  w <- as.numeric(w)
  h <- as.numeric(h)
})

dims <- within(dims, {
  area <- 2*l*w + 2*w*h + 2*h*l
  face_1 <- l * w
  face_2 <- w * h
  face_3 <- h * l
  
  min <- pmin(face_1, face_2, face_3)
  total <- area + min
}
)

sum(dims$total)


## Part 2 ----

dims <- within(dims, {
  p1 <- 2*l + 2*w
  p2 <- 2*w + 2*h
  p3 <- 2*h + 2*l
  pmin <- pmin(p1, p2, p3)
  vol <- l*w*h
  
  ribbon <- pmin + vol
})

sum(dims$ribbon)
