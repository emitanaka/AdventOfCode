library(tidyverse)
library(unglue)
input <- read_lines("2020/inputs/day8_input.txt")

df <- input %>%
  unglue_data("{op} {move}") %>%
  mutate(move = as.numeric(move)) %>%
  as_tibble()

# part 1 ------------------------------------------------------------------

accumulator <- 0
line <- 1
visit <- integer()
while(!line %in% visit) {
  visit <- c(visit, line)
  op <- df[line, ]$op
  move <- df[line, ]$move
  line <- case_when(op == "jmp" ~ line + move,
                    op %in% c("nop", "acc") ~ line + 1)
  accumulator <- accumulator + ifelse(op=="acc", move, 0)
}

accumulator


# part 2 ------------------------------------------------------------------

index_jmp <- which(df$op=="jmp")
index_nop <- which(df$op=="nop")
index <- c(index_jmp, index_nop)
jmp2nop <- rep(c("jmp", "nop"), c(length(index_jmp), length(index_nop)))

for(i in 1:length(index)) {
  tmp_df <- df
  tmp_df[index[i], "op"] <- setdiff(c("jmp", "nop"), jmp2nop[i])
  accumulator <- 0
  line <- 1
  visit <- nrow(df)
  while(!line %in% visit) {
    visit <- c(visit, line)
    op <- tmp_df[line, ]$op
    move <- tmp_df[line, ]$move
    line <- case_when(op == "jmp" ~ line + move,
                      op %in% c("nop", "acc") ~ line + 1)
    accumulator <- accumulator + ifelse(op=="acc", move, 0)
  }
  if(line==nrow(df)) break
}
