circuit <- readLines('2015/d7_input.txt')


`%AND%` <- function(a, b) bitwAnd(a, b)
circuit <- gsub('AND', '%AND%', circuit)

`%NOT%` <- function(a, b) bitwNot(a, b)
circuit <- gsub('NOT', '%NOT%', circuit)

`%OR%` <- function(a, b) bitwOr(a, b)
circuit <- gsub('OR', '%OR%', circuit)

`%RSHIFT%` <- function(a, b) bitwShiftR(a, b)
circuit <- gsub('RSHIFT', '%RSHIFT%', circuit)

`%LSHIFT%` <- function(a, b) bitwShiftL(a, b)
circuit <- gsub('LSHIFT', '%LSHIFT%', circuit)

# starting values are those that start with a number then have an assignment
# operator
st_val <- grep('^\\d+ ->', circuit, value = T)
eval(parse(text = st_val))

vars <- gsub('.*-> ', '', st_val)

# val %% number
val_op_num <- NULL
for(i in 1:length(vars)){
  val_op_num <- c(val_op_num,
                grep(paste0('^', vars[i], ' .* \\d'),
                     circuit, value = T))
} 

eval(parse(text = val_op_num))

vars <- c(vars, gsub('.*-> ', '', val_op_num))

# NOT something
not_val <- NULL
for(i in 1:length(vars)){
  not_val <- c(not_val,
               grep(paste0('%NOT', vars[i]),
                    circuit, value = T))

} 


1) assign numbers to something

 something % number
 not something

