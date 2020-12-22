library(tidyverse)

input <- read_lines("2020/inputs/day13_input.txt")

timestamp <- as.numeric(input[1])
roll <- input[2] %>%
  str_split(",") %>%
  pluck(1)
buses <- roll %>%
  setdiff("x") %>%
  as.numeric()

# part 1 ------------------------------------------------------------------

step <- timestamp
repeat {
  time <- map_dbl(buses, ~step %% .x)
  if(any(time==0)) break
  step <- step + 1
}

buses[which(time==0)] * (step - timestamp)

# part 2 ------------------------------------------------------------------

offset <- which(roll!="x") - 1
step <- buses[1]
repeat {
    wait <- map2_dbl(step + offset, buses, ~.x %% .y)
    if(all(wait==0)) break
    step <- step + prod(buses[wait==0])
}

options(scipen = 14)
step
