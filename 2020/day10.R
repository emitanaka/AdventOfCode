library(tidyverse)

x <- read_lines("2020/inputs/day10_input.txt") %>%
  as.numeric() %>%
  sort()
x2 <- c(0, x, max(x) + 3)

# part 1 ------------------------------------------------------------------

prod(table(diff(x2)))

# part 2 ------------------------------------------------------------------

options("scipen" = 14)
rle(diff(x2)) %>%
  unclass() %>%
  as.data.frame() %>%
  filter(values == 1) %>%
  mutate(comb = map_dbl(lengths, ~sum(choose(.x - 1, 0:2)))) %>%
  pull(comb) %>%
  prod()

