library(data.table)

elf_assign <- fread('2022/d4_input.txt',
                    col.names = c('e1', 'e2'),
                    header = F)

elf_assign[, ':='(
  e1_start = as.numeric(gsub('-.*', '', e1)),
  e1_end = as.numeric(gsub('.*-', '', e1)),
  e2_start = as.numeric(gsub('-.*', '', e2)),
  e2_end = as.numeric(gsub('.*-', '', e2)),
  group = as.factor(1:nrow(elf_assign))
)]

elf1 <- elf_assign[, grep('e1|g', names(elf_assign), value=T), with = F]
setkey(elf1, group, e1_start, e1_end)
elf2 <- elf_assign[, grep('e2|g', names(elf_assign), value=T), with = F]
setkey(elf2, group, e2_start, e2_end)

e2_contains <- foverlaps(elf1, elf2, type = 'within',
          nomatch = 0)
e1_contains <- foverlaps(elf2, elf1, type = 'within',
          nomatch = 0)

length(unique(c(e1_contains$group, e2_contains$group)))


# Pt 2 ----
elf_overlaps <- foverlaps(elf1, elf2, type = 'any',
                         nomatch = 0)
