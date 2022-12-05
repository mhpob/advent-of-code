library(data.table)

guide <- fread('day 2/day2_input.txt',
                  header = F)
# First column is them, second is me
setnames(guide, c('them', 'me'))

## Part 1 ----
# Assumption is that:
#   A = Rock, B = Paper, C = Scissors
#   X = Rock, Y = Paper, Z = Scissors

# Make a key
key <- data.table(
  expand.grid(
    them = c('r', 'p', 's'),
    me = c('r', 'p', 's')
  )
)

# Assign points
key[, outcome := fcase(
  them == me, 3,
  (them == 'r' & me == 'p') |
    (them == 'p' & me == 's') |
    (them == 's' & me == 'r'), 6,
  default = 0
)]


# Rename a/b/c, x/y/z for clarity: apply switch by row
guide[, them := switch(them,
                       A = 'r',
                       B = 'p',
                       C = 's'),
      by = them]
guide[, me := switch(me,
                       X = 'r',
                       Y = 'p',
                       Z = 's'),
      by = me]

# Join the key
guide <- key[guide, on = c('them', 'me')]

# Point value for shape
guide[, shape := fcase(
  me == 'r' , 1,
  me == 'p', 2,
  me == 's', 3
)]

guide[, round := outcome + shape]

guide[, .(sum(round))]

# Part 2 ----
guide <- fread('day 2/day2_input.txt',
               header = F)
setnames(guide, c('them', 'outcome'))

# Reality is that:
#   A = Rock, B = Paper, C = Scissors
#   X = lose, Y = draw, Z = win


guide[, them := switch(them,
                       A = 'r',
                       B = 'p',
                       C = 's'),
      by = them]

# Instead of x/y/z being me, make it the outcome
guide[, outcome := switch(outcome,
                     X = 0,
                     Y = 3,
                     Z = 6),
      by = outcome]
guide[, outcome := as.numeric(outcome)]

# Join the key
guide <- key[guide, on = c('them', 'outcome')]

# Apply shape point values
guide[, shape := fcase(
  me == 'r' , 1,
  me == 'p', 2,
  me == 's', 3
)]


guide[, round := outcome + shape]

guide[, .(sum(round))]
