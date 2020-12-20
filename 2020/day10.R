library(tidyverse)

x <- read_lines("2020/inputs/day10_input.txt") %>%
  as.numeric() %>%
  sort()
x2 <- c(0, x, max(x) + 3)

# part 1 ------------------------------------------------------------------

prod(table(diff(x2)))

# part 2 ------------------------------------------------------------------
# if only 1 diff=1 in a row, then one way
sum(choose(1, 0:1)) # if 2 diff=1 in a row, then one must be picked,
                    # the other can be removed or not
sum(choose(2, 0:2)) # for 3, then 0, 1 or 2 can be removed
sum(choose(3, 0:2)) # for 4, then 0, 1 or 2 can be removed.
                    # 3 cannot be removed, because one needed for diff <= 3.
combs <- c(1, 2, 4, 7)

options("scipen" = 14)
rle(diff(x2)) %>% # diff values are 1 or 3
  unclass() %>%
  as.data.frame() %>% # when diff is 3, there is no choice
  filter(values == 1) %>%
  mutate(comb = combs[lengths]) %>%
  pull(comb) %>%
  prod()

