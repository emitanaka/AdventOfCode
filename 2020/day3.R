library(tidyverse)

x <- read_lines("2020/inputs/day3_input.txt")


# part 1 ------------------------------------------------------------------

lat <- 1
lon <- 1
count <- 0
for(lat in 1:length(x)) {
  line <- str_dup(x[[lat]], ceiling(lon / str_length(x[[lat]])))
  count <- count + as.numeric(str_sub(line, lon, lon) == "#")
  lat <- lat + 1
  lon <- lon + 3
}

count

# part 2 ------------------------------------------------------------------

out <- map2_dbl(c(1, 3, 5, 7, 1),
                c(1, 1, 1, 1, 2),
                 ~{
                     lat <- 1
                     lon <- 1
                     count <- 0
                     while(lat <= length(x)) {
                       line <- str_dup(x[[lat]], ceiling(lon / str_length(x[[lat]])))
                       count <- count + as.numeric(str_sub(line, lon, lon) == "#")
                       lat <- lat + .y
                       lon <- lon + .x
                     }
                     count
                 })

prod(out)
