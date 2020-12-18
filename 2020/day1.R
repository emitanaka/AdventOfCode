library(tidyverse)

x <- read_lines("2020/inputs/day1_input.txt") %>%
  as.numeric()
n <- length(x)

# part 1 ------------------------------------------------------------------

for(i in 1:(n - 1)) {
  for(j in (i + 1):n) {
    if(x[i] + x[j] == 2020) break
  }
  if(x[i] + x[j] == 2020) break
}

x[i] * x[j]


# part 2 ------------------------------------------------------------------

for(i in 1:(n - 2)) {
  for(j in (i + 1):(n - 1)) {
    for(k in (j + 1):n) {
      if(x[i] + x[j] + x[k] == 2020) break
    }
    if(x[i] + x[j] + x[k] == 2020) break
  }
  if(x[i] + x[j] + x[k] == 2020) break
}

x[i] * x[j] * x[k]
