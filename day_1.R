library(readr)
library(dplyr)
df<-readr::read_delim("inputs/1a.txt", delim= "   ", col_names = FALSE)

col1 <- sort(df$X1)
col2 <- sort(df$X2)

df2<-tibble(col1, col2) |>
  mutate(min= abs(col2-col1))

sum(df2$min)

diff = 0 
for (i in col1){
  
  diff_add = i * length(which(col2 == i))
  diff = diff + diff_add
}
print(diff)
