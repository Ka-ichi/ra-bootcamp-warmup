library(tidyverse)

#カレントディレクトリを指定
setwd("C:/Users/tryca/R_files/ra_bootcamp/ra-bootcamp-warmup/assignment_1")

class_dir = "data/raw/学級数"
class_files = list.files(class_dir, full.names = TRUE)

class_files_name <-
  list.files(class_dir, full.names = F) |>
  str_remove(".xlsx")


list_df_class_raw <- 
  map(class_files, read_excel) |>
  setNames(class_files_name)

list_df_class_clean <- list_df_class_raw |>
  map(~ .x |> rename_with(~ str_remove(.x, "^\\.\\.+")))

#のちに1行目を列名に変更するため、ここで変数の変更は不要と判断した。

saveRDS(list_df_class_clean, file = "data/original/class_list.rds")


