library(tidyverse)

input <- read_lines("2020/inputs/day11_input.txt") %>%
  str_split("") %>%
  do.call("rbind", .)

nr <- nrow(input)
nc <- ncol(input)
states <- c("L", "#", ".")

# part 1 ------------------------------------------------------------------

old <- matrix("L", nr, nc)
new <- input
while(!all(old==new)) {
  old <- new
  for(i in 1:nr) {
    for(j in 1:nc) {
      rows <- seq(max(1, i - 1), min(i + 1, nr))
      cols <- seq(max(1, j - 1), min(j + 1, nc))
      seats <- factor(old[rows, cols], levels = states)
      seat <- factor(old[i, j], levels = states)
      tt <- table(seats) - table(seat)
      new[i, j] <- case_when(seat=="L" & tt["#"]==0 ~ "#",
                             seat=="#" & tt["#"]>=4 ~ "L",
                             TRUE ~ as.character(seat))
    }
  }
}

table(new)["#"]

# pretty sure I can make the above code shorter by recognising that
# some places are obviously going to be occupied in the end

# part 2 ------------------------------------------------------------------

old <- matrix(".", nr, nc)
new <- input
f <- function(x) {
    res <- str_c(x, collapse = "") %>%
      str_extract("[L#]")
    ifelse(is.na(res), ".", res)
}
while(!all(old==new)) {
  old <- new
  for(i in 1:nr) {
    for(j in 1:nc) {
      seat <- factor(old[i, j], levels = states)
      if(seat==".") next
      NW <- ifelse(i==1 | j==1, ".", f(diag(old[(i - 1):1, (j - 1):1, drop = FALSE])))
      NE <- ifelse(i==1 | j==nc, ".", f(diag(old[(i - 1):1, (j + 1):nc, drop = FALSE])))
      SE <- ifelse(i==nr | j==nc, ".", f(diag(old[(i + 1):nr, (j + 1):nc, drop = FALSE])))
      SW <- ifelse(i==nr | j==1, ".", f(diag(old[(i + 1):nr, (j - 1):1, drop = FALSE])))
      N <- ifelse(i==1, ".", f(old[(i - 1):1, j]))
      E <- ifelse(j==nc, ".", f(old[i, (j + 1):nc]))
      S <- ifelse(i==nr, ".", f(old[(i + 1):nr, j]))
      W <- ifelse(j==1, ".", f(old[i, (j - 1):1]))

      seats <- factor(c(NW, NE, SE, SW, N, E, S, W), levels = states)
      tt <- table(seats)
      new[i, j] <- case_when(seat=="L" & tt["#"]==0 ~ "#",
                             seat=="#" & tt["#"]>=5 ~ "L",
                             TRUE ~ as.character(seat))
    }
  }
}

table(new)["#"]
