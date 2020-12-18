library(tidyverse)

input <- read_lines("2020/inputs/day12_input.txt")
mat <- str_match(input, "^([NSWELRF])([0-9]+)")
df <- tibble(dir = mat[, 2],
             move = as.numeric(mat[, 3]))

# part 1 ------------------------------------------------------------------

lat <- lon <- 0
degree <- 90

for(i in 1:nrow(df)) {
  dir <- df$dir[i]
  move <- df$move[i]

  degree <- case_when(dir=="L" ~ (degree - move) %% 360,
                      dir=="R" ~ (degree + move) %% 360,
                      TRUE ~ degree)

  lat <- case_when(dir=="N" ~ lat + move,
                   dir=="S" ~ lat - move,
                   dir=="F" & degree==0 ~ lat + move,
                   dir=="F" & degree==180 ~ lat - move,
                   TRUE ~ lat)

  lon <- case_when(dir=="E" ~ lon + move,
                   dir=="W" ~ lon - move,
                   dir=="F" & degree==90 ~ lon + move,
                   dir=="F" & degree==270 ~ lon - move,
                   TRUE ~ lon)
}

abs(lat) + abs(lon)

# part 2 ------------------------------------------------------------------

position <- 0 + 0i
waypoint <- 10 + 1i
for(i in 1:nrow(df)) {
  dir <- df$dir[i]
  move <- df$move[i]
  waypoint <- case_when(dir=="L" ~ waypoint * complex(argument = move/180 * pi),
                        dir=="R" ~ waypoint * complex(argument = -move/180 * pi),
                        dir=="N" ~ waypoint + complex(real = 0, imaginary = move),
                        dir=="S" ~ waypoint + complex(real = 0, imaginary = -move),
                        dir=="W" ~ waypoint + complex(real = -move, imaginary = 0),
                        dir=="E" ~ waypoint + complex(real = move, imaginary = 0),
                        TRUE ~ waypoint)
  if(dir=="F") position <- position + waypoint * move
}

abs(Re(position)) + abs(Im(position))

