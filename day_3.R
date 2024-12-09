library(readr)
library(dplyr)
library(stringr)
txt<-readr::read_file("inputs/day_3.txt")


match_list=str_match_all(txt, "(?:mul\\()(\\d{1,5},\\d{1,5})(?:\\))")
multiplying_numbers <- match_list[[1]][,2]



df<- as_tibble(str_split_fixed(multiplying_numbers, ",", 2))|>
  mutate(V1= as.numeric(V1), V2= as.numeric(V2)) |>
  mutate(mult_result = V1 * V2)


sum(df$mult_result)
