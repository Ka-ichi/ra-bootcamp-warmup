#options(repos="https://ftp.yz.yamagata-u.ac.jp/pub/cran/")
library(tidyverse)
library(readxl)


#カレントディレクトリを指定
setwd("C:/Users/tryca/R_files/ra_bootcamp/ra-bootcamp-warmup/assignment_1")

#不登校生徒数のファイル一覧を取得
absence_dir <- "data/raw/不登校生徒数/"
absence_files <- list.files(absence_dir, full.names = TRUE)

for (file in absence_files) {
  year <- str_extract(basename(file), "\\d{4}")
  assign(paste0("absence_", year), read_excel(file))
}

#生徒数のデータフレーム作成
students_path <- "data/raw/生徒数/生徒数.xlsx"
students_df <- read_excel(students_path)


for (yr in 2013:2022) {
  df_name <- paste0("absence_", yr)
  
  # データフレームを取得
  df <- get(df_name)
  
  # 変数名を変更
  df <- df %>%
    rename(
      prefecture = "都道府県",
      no_school  = "不登校生徒数"
    )
  
  # 上書き保存
  assign(df_name, df)
}

students_df <- students_df %>%
  rename(
    prefecture  = "都道府県",
    year        = "年度",
    num_student = "生徒数"
  )

absence_list <- mget(paste0("absence_", 2013:2022)) %>%
  setNames(sub("^absence_", "", names(.)))

#rds形式で保存
saveRDS(students_df, file = "data/original/students_df.rds")
# 2013〜2022年の absence_XXXX を順に保存
for (yr in 2013:2022) {
  df_name <- paste0("absence_", yr)  
  df <- get(df_name)                   
  save_path <- paste0("data/original/", df_name, ".rds")
  saveRDS(df, file = save_path)
}

# absence_list を保存
saveRDS(absence_list, file = "data/original/absence_list.rds")
