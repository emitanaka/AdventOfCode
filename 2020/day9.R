library(tidyverse)
input <- read_lines("2020/inputs/day9_input.txt") %>% as.numeric()

preamble <- 25

# part 1 ------------------------------------------------------------------

for(ind in seq(preamble + 1, length(input))) {
  x <- input[seq(ind - 25, ind - 1)]
  for(i in 1:24) {
    for(j in 2:25) {
      valid <- x[i] + x[j] == input[ind]
      if(valid) break
    }
    if(valid) break
  }
  if(!valid) break
}

out <- input[ind]
out


# part 1 alternative ------------------------------------------------------

ind <- 591
win <- seq(ind - 24, ind)
M <- outer(input[win], input[win], "+")
if(any(M==input[ind + 1])) input[ind + 1]


# part 2 ------------------------------------------------------------------

n <- length(input)
for(l in seq(2, n)) {
  for(ind in seq(1, n - l + 1)) {
    x <- input[seq(ind, ind + l - 1)]
    found <- sum(x) == out
    if(found) break
  }
  if(found) break
}

min(x) + max(x)
