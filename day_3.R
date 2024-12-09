library(readr)
library(dplyr)
library(stringr)
txt<-readr::read_file("inputs/day_3.txt")

get_mult_sum<-function(txt){
  match_list=str_match_all(txt, "(?:mul\\()(\\d{1,5},\\d{1,5})(?:\\))")
  
  multiplying_numbers <- match_list[[1]][,2]
  
  df<- as_tibble(str_split_fixed(multiplying_numbers, ",", 2))|>
    mutate(V1= as.numeric(V1), V2= as.numeric(V2)) |>
    mutate(mult_result = V1 * V2)
  
  
  return(sum(df$mult_result))
  }

get_mult_sum(txt)

my_strings=str_split(txt, "(?<=do(n't)?\\(\\))")[[1]]

df_2 <- tibble(my_strings) |>
  mutate(has_dont = str_detect(my_strings, "don't\\(\\)"),
         ignore = lag(has_dont)) |>
  filter(is.na(ignore) | !ignore)


my_strings_do <-str_c(df_2$my_strings, collapse = " ")


get_mult_sum(my_strings_do)

