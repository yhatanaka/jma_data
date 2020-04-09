library("tidyverse")
library("magrittr")

data_folder <- './'
read_jma <- function(csv_f) {
  f <-
    read_csv(
      csv_f
      , col_names = FALSE
      , skip = 1
      , locale = locale(encoding = "cp932")
    )
  # 空行削除
  return(f %>% filter(rowSums(is.na(.)) != ncol(.)))
}

data_files <- list.files(data_folder, full.names = T, pattern = '.csv')
moddata_list <- data_files %>% lapply(read_jma)
moddata_list

