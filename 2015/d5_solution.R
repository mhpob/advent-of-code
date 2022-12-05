
nn_list <- read.delim('2015/d5_input.txt',
                      header = F,
                      col.names = 'string')

nn_list <- within(nn_list,
                  {
                  v3 = gsub('[^aeiou]', '', string)
                  v3 = nchar(v3)
                  v3 = v3 >= 3
                  }
)

nn_list <- within(nn_list,
                  {
                    twice = 
                        regmatches(
                          string,
                          gregexpr('(.)\\1', string)
                        )
                  })

nn_list <- within(nn_list,
                  {
                    twice = 
                      apply(nn_list, 1, function(.){
                        length(paste(.$twice)) != 0
                      })
                  })

nn_list <- within(nn_list,
                  {
                    good = !grepl('ab|cd|pq|xy', string)
                  }
)

nn_list <- within(nn_list,
                  {
                    nice = apply(nn_list[, 2:4], 1, all)
                  }
)

sum(nn_list$nice)

## Pt 2 ----



nn_list <- within(nn_list, {
  # .* matches 0+ characters before (lets us begin anywhere)
  #   \w{2} matches any two-letter "word character";
  #   placing it within parentheses assigns ("captures") it to "1" ;
  #   .* allows 0+ characters before...
  #   \1 has it match the captured pattern again
  
  paired = grepl('.*(\\w{2}).*\\1', string)
  
  # .* matches 0+ characters before (lets us begin anywhere)
  #   \w matches any "word character";
  #   placing it within parentheses assigns ("captures") it to "1" ;
  #   . allows 1 character before...
  #   \1 has it match the captured pattern again
  repeats = grepl('.*(\\w).\\1', string)
})

nn_list <- within(nn_list,
                  {
                    nice2 = apply(nn_list[, 6:7], 1, all)
                  }
)

sum(nn_list$nice2)
