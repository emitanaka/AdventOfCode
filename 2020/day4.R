library(tidyverse)

x <- read_file("2020/inputs/day4_input.txt")
passports <- str_split(x, "\\n\\n") %>%
  pluck(1) %>%
  str_replace_all("\\n", " ") %>%
  str_match_all("([a-z]{3}):([\\S]+)")

df <- map_dfr(passports, ~{
  .x[, 2:3] %>%
    as.data.frame() %>%
    pivot_wider(names_from = V1, values_from = V2)
})

# part 1 ------------------------------------------------------------------

mss <- df %>%
  select(-cid) %>%
  is.na() %>%
  rowSums()

sum(mss==0)

# part 2 ------------------------------------------------------------------

mss <- df %>%
  mutate(byr = as.numeric(byr),
         iyr = as.numeric(iyr),
         eyr = as.numeric(eyr),
         hgt_cm = as.numeric(str_match(hgt, "^([0-9]+)cm")[, 2]),
         hgt_in = as.numeric(str_match(hgt, "^([0-9]+)in")[, 2])) %>%
  filter(between(byr, 1920, 2002),
         between(iyr, 2010, 2020),
         between(eyr, 2020, 2030),
         between(hgt_cm, 150, 193) | between(hgt_in, 59, 76),
         str_detect(hcl, "^#[0-9a-f]{6}") & str_length(hcl) == 7,
         ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
         str_detect(pid, "^[0-9]{9}") & str_length(pid) == 9) %>%
  select(-c(cid, hgt_cm, hgt_in)) %>%
  is.na() %>%
  rowSums()

sum(mss==0)
