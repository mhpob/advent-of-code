library(data.table)

# read in data (I have it saved in a folder called "day 1")
elf_cals <- fread('day 1/day1_input.txt')
# Peek at data (doing rows 10-20 since my first group isnt until row 15)
elf_cals[10:20]

# Blank lines are converted to NA when I read it in. Make another column
#   where a number is "TRUE" and an NA is "FALSE"
elf_cals[, elf_delim := fifelse(is.na(V1), T, F)]
# Peek
elf_cals[10:20]

# Use run-length type group ID. This will assign a group to each run of the
#   same value. So FFF is 1, then a T would be 2, then FFFFFFF would be 3, etc.
elf_cals[, elf := rleid(elf_delim)]
# Peek
elf_cals[10:20]

# It just so happens that all of the odd numbers are the data that I want, and
#   all of the even numbers are the original NAs. I don't think there's an 
#   "is.odd" or "is.even" function, so I'm using the modulus. Basically, if you
#   divide the group number by 2 and there is a remainder of 1, it's odd.
elf_cals <- elf_cals[elf %% 2 == 1]
# Peek
elf_cals[10:20]

# Now I'm going to sum by group (i.e. sum by elf)
elf_cals <- elf_cals[, .(total_cals = sum(V1)), by = elf]
# Peek
elf_cals[10:20]

# And now find which row has the maximum total calories
elf_cals[which.max(total_cals)]

# My answer is 74198


## Part 2 ---
elf_cals[order(-total_cals), sum(total_cals[1:3])]
