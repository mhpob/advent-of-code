cal <- read.table('2023/d1_data.txt',
                  col.names = 'bad_vals')

# Part 1
cal$num <- gsub('[^0-9]', '', cal$bad_vals)

my_fun <- Vectorize(
  function(.){
    hold <- unlist(strsplit(., ''))
    paste(hold[c(1, length(hold))], collapse = '')
  }
)

cal$val <- my_fun(cal$num)
cal$val <- as.numeric(cal$val)
sum(cal$val)
#55386

# Part 2
char_numbers <- data.frame(
  text = c(
    'one',
    'two',
    'three',
    'four',
    'five',
    'six',
    'seven',
    'eight',
    'nine'
  ),
  int = 1:9
)

cal$start_num <- sub(
  '.*(([0-9]|one|two|three|four|five|six|seven|eight|nine).*?).*',
  '\\1', 
  cal$bad_vals
)
cal$end_num <- sub(
  '.*(([0-9]|one|two|three|four|five|six|seven|eight|nine).*?)$',
  '\\2',
  cal$bad_vals
)

cal <- merge(cal, char_numbers, by.x = 'start_num', by.y = 'text',
             all.x = TRUE)
cal <- merge(cal, char_numbers, by.x = 'end_num', by.y = 'text',
             all.x = TRUE)

cal$int.x[is.na(cal$int.x)] <- cal$start_num[is.na(cal$int.x)]
cal$int.y[is.na(cal$int.y)] <- cal$end_num[is.na(cal$int.y)]

cal$val2 <- as.numeric(paste0(cal$int.x, cal$int.y))
sum(cal$val2)
# 54824