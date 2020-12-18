library(tidyverse)

x <- read_lines("2020/inputs/day5_input.txt")


# part 1 ------------------------------------------------------------------


seat_id <- vector("numeric", length(x))
for(iline in 1:length(x)) {
  .x <- x[iline]
  row_start <- 0
  row_end <- 127
  for(i in 1:7) {
    pos <- str_sub(.x, i, i)
    if(pos=="B") row_start <- row_start + (row_end - row_start + 1) / 2
    if(pos=="F") row_end <- row_end - (row_end - row_start + 1) / 2
  }
  row <- row_start

  col_start <- 0
  col_end <- 7
  for(i in 8:10) {
    pos <- str_sub(.x, i, i)
    if(pos=="R") col_start <- col_start + (col_end - col_start + 1) / 2
    if(pos=="L") col_end <- col_end - (col_end - col_start + 1) / 2
  }
  col <- col_start

  seat_id[iline] <- 8 * row + col
}

max(seat_id)


# part 2 ------------------------------------------------------------------

row <- 1:126
col <- 0:7
seat_id_all <- expand_grid(row = 1:126, col = 0:7) %>%
  mutate(seat_id = 8 * row + col) %>%
  pull(seat_id)

empty_seats <- setdiff(seat_id_all, seat_id)
lag(empty_seats, 1)
tibble(empty = empty_seats) %>%
  mutate(lag = empty - lag(empty, 1),
         lead = lead(empty, 1) - empty,
         occupied = !(lead!=1 & lag!=1)) %>%
  filter(!occupied) %>%
  pull(empty)
