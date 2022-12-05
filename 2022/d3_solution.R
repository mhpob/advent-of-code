library(data.table)

sack <- fread('2022/d3_input.txt',
              header = F,
              col.names = 'items')

##PT 1----
sack[, n_items := nchar(items)]
sack[, ':='(comp1 = substr(items, 1, n_items/2),
            comp2 = substr(items, n_items/2 + 1, n_items))]

sack[, comp1 := strsplit(comp1, '')]
sack[, comp2 := strsplit(comp2, '')]

sack[, shared := intersect(unlist(comp1), unlist(comp2)), by = items]

sack[, priority := ifelse(shared %in% letters,
                       which(letters == shared),
                       which(LETTERS == shared) + 26),
     by = items]

sack[, .(sum(priority))]


# PT 2 ----
sack[, group := rep(seq(1, nrow(sack)/3, 1), each = 3)]
sack[, elf := rep(seq(1, 3, 1), times = nrow(sack)/3)]

sack2 <- dcast(sack, group~elf, value.var = 'items')

sack2 <- sack2[, .(badge = intersect(
  intersect(
    unlist(strsplit(`1`, '')),
    unlist(strsplit(`2`, ''))
  ),
  unlist(strsplit(`3`, ''))
)
), by=group]


sack2[, priority := ifelse(badge %in% letters,
                          which(letters == badge),
                          which(LETTERS == badge) + 26),
     by = group]

sack2[, .(sum(priority))]
