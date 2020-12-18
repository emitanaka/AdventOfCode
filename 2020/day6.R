library(tidyverse)

dat <- read_file("2020/inputs/day6_input.txt")


# part 1 ------------------------------------------------------------------

dat %>%
  str_split("\\n\\n") %>%
  pluck(1) %>%
  str_replace_all("\\n", "") %>%
  str_split("") %>%
  map_int(n_distinct) %>%
  sum()

# part 2 ------------------------------------------------------------------

dat %>%
  str_split("\\n\\n") %>%
  pluck(1) %>%
  str_split("\\n") %>%
  map_int(~{
    str_subset(.x, "") %>%  # remove the empty string
      str_split("") %>%
      reduce(intersect) %>%
      n_distinct()
  }) %>%
  sum()



