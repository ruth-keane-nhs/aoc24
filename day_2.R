library(readr)
library(dplyr)
library(stringr)
df<-readr::read_delim("inputs/day_2.txt", col_names = FALSE, delim="\t")

                      
safe_reports = 0
for ( i in 1:nrow(df)){
  i_vec <-as.numeric(str_split(df$X1[i], " ")[[1]])
  diff_vec <- diff(i_vec)
  difference_range_ok <- sum(abs(diff_vec) <= 3 & abs(diff_vec) >= 1)==length(diff_vec)
  direction_ok <- length(unique(sign(diff_vec)))==1
  if(direction_ok & difference_range_ok){
    safe_reports = safe_reports + 1 
  }
}

check_safe<-function(vec_to_check){
  diff_vec <- diff(vec_to_check)
  difference_range_ok <- sum(abs(diff_vec) <= 3 & abs(diff_vec) >= 1)==length(diff_vec)
  direction_ok <- length(unique(sign(diff_vec)))==1
  ok_no_changes = direction_ok & difference_range_ok
  return(ok_no_changes)
}


safe_reports = 0
for ( i in 1:nrow(df)){
  i_vec <-as.numeric(str_split(df$X1[i], " ")[[1]])
  ok_no_changes <-check_safe(i_vec)
  if(ok_no_changes){
    safe_reports = safe_reports + 1 
  }else{
    safe_once_removed = 0
    for(j in 1:length(i_vec)){
      remove_vec <- i_vec[-j]
      safe_once_removed = safe_once_removed + check_safe(remove_vec)
    }
    if(safe_once_removed>0){
      safe_reports= safe_reports + 1
    }
  }
  
  
  
 
}

