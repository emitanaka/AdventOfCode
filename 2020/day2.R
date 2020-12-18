library(tidyverse)

x <- read_lines("2020/inputs/day2_input.txt")
mat <- str_match(x, "^(\\d+)-(\\d+) ([a-z]): (.+)")


# part 1 ------------------------------------------------------------------

df <- tibble(min = as.numeric(mat[, 2]),
             max = as.numeric(mat[, 3]),
             letter = mat[, 4],
             word = mat[, 5])

df %>%
  rowwise() %>%
  mutate(count = str_count(word, letter),
         match = between(count, min, max)) %>%
  pull(match) %>%
  sum()

# part 2 ------------------------------------------------------------------

df %>%
  rowwise() %>%
  mutate(pattern1 = str_c("^", str_dup("[a-z]", min - 1), letter),
         pattern2 = str_c("^", str_dup("[a-z]", max - 1), letter),
         pos1 = str_detect(word, pattern1),
         pos2 = str_detect(word, pattern2),
         detect = pos1 + pos2) %>%
  filter(detect==1) %>%
  nrow()
