series <- read.table('2022/d9_input.txt', header = F,
                     col.names = c('dir', 'step'))

library(data.table)

series<- fread('2022/d9_input.txt', header = F,
                    col.names = c('dir', 'step'))
series <- rbind(
  # initial conditions
  data.table(dir = NA, step = NA, h_x = 0, h_y = 0, t_x = 0, t_y = 0),
  # input data
  series,
  fill = T
)

#Heads path
series[-1, ':='(h_x = fcase(dir == 'R', series[.I - 1, h_x] + step,
                            dir == 'L', series[.I - 1, h_x] - step,
                            dir %in% c('D', 'U'), series[.I - 1, h_x]),
                h_y = fcase(dir == 'U', series[.I - 1, h_y] + step,
                            dir == 'D', series[.I - 1, h_y] - step,
                            dir %in% c('L', 'R'), series[.I - 1, h_y])),
                by = .I]


copy(series)[-1, ':='(t_x = fifelse(
  abs(abs(h_x) - abs(series[.I-1,t_x])) <= 1, series[.I-1,t_x], 
  ifelse(h_x < 0))
),
by = .I]

