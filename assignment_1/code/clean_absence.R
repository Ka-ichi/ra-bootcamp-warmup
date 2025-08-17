library(tidyverse)
library(dplyr)
library(readr)
library(purrr)

student_df <- readRDS("data/original/students_df.rds") 


absence_list <- readRDS("data/original/absence_list.rds")

absence_df <- absence_list %>%
  bind_rows(.id = "year") %>%
  select(-blank) %>%
  mutate(
    year       = as.integer(year),              
    no_school  = as.integer(no_school),         
  )

student_df <- student_df |>
  mutate(
    year = as.integer(year),
    num_student = as.integer(num_student)
  )

joint_df <- student_df %>%
  left_join(absence_df, by = c("prefecture", "year")) %>%
  mutate(
    no_school_rate = if_else(no_school > 0,
                             no_school / num_student,
                             NA_real_), #実数型に対応するため
    no_school_rate = no_school_rate * 100
  ) %>%
  write_rds("data/cleaned/students_absence_2013_2022.rds")











