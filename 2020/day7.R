library(tidyverse)
library(unglue)
dat <- read_lines("2020/inputs/day7_input.txt")


# part 1 ------------------------------------------------------------------

rules <- list()
for(.x in dat) {
  bag <- str_match(.x, "^(\\S+ \\S+) bags")[, 2]
  rules[[bag]] <- str_split(.x, " contain ") %>%
    pluck(1, 2) %>%
    str_split(", ") %>%
    pluck(1) %>%
    str_match("(\\d+) (\\S+ \\S+) bag") %>%
    .[, 3]
}
vec <- "shiny gold"
bags <- character()
while(length(vec) > 0) {
  vec <- names(rules)[map_lgl(rules, ~any(vec %in% .x))]
  bags <- c(bags, vec)
}

n_distinct(bags)


# part 2 ------------------------------------------------------------------

patterns <- c("{parent} bags contain no other bags.",
              "{parent} bags contain {n1} {color1} bag{}, {n2} {color2} bag{}, {n3} {color3} bag{}, {n4} {color4} bag{}.",
              "{parent} bags contain {n1} {color1} bag{}, {n2} {color2} bag{}, {n3} {color3} bag{}.",
              "{parent} bags contain {n1} {color1} bag{}, {n2} {color2} bag{}.",
              "{parent} bags contain {n1} {color1} bag{}.")

df <- dat %>%
  unglue_data(patterns) %>%
  mutate(child1 = str_c(color1, n1, sep = ":"),
         child2 = str_c(color2, n2, sep = ":"),
         child3 = str_c(color3, n3, sep = ":"),
         child4 = str_c(color4, n4, sep = ":")) %>%
  select(parent, child1:child4) %>%
  pivot_longer(-parent,
               names_to = "name",
               values_to = "value") %>%
  filter(!is.na(value)) %>%
  separate(value, into = c("child", "n"), sep = ":", convert = TRUE)

count_child <- function(aparent) {
  sub <- filter(df, parent==aparent)
  if(nrow(sub))
    sum(sub$n) + sum(map_dbl(sub$child, count_child) * sub$n)
  else
    0
}

count_child("shiny gold")
