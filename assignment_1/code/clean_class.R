library(tidyverse)
library(Hmisc)

#カレントディレクトリを指定
setwd("C:/Users/tryca/R_files/ra_bootcamp/ra-bootcamp-warmup/assignment_1")

class_list <- readRDS("data/original/class_list.rds")
solution <- readRDS("C:/Users/tryca/Downloads/課題2_完成版データ.rds")


era_to_ad <- function(x) {
  x <- as.character(x)
  x_clean <- stringr::str_replace(x, "年度", "")
  dplyr::case_when(
    stringr::str_detect(x_clean, "平成") ~ as.integer(stringr::str_extract(x_clean, "\\d+")) + 1988,
    stringr::str_detect(x_clean, "令和") ~ as.integer(stringr::str_extract(x_clean, "\\d+")) + 2018,
    TRUE ~ NA_integer_
  ) |> as.character()
}

class_list_named <- class_list |>
  set_names(map_chr(class_list, \(df) era_to_ad(df[[1]][1])))



class_list_named <- class_list_named |>
  map(\(df) {
    new_names <- df[1, ] |>
      as.character() |>
      str_replace("学級", "")
    
    df |>
      set_names(new_names) |>     # 列名を更新
      slice(-1) |>
      select(-計) |>
      rename(prefecture = 1) 
  })

#リストをひとつにまとめ、要素名をyear列に追加
df_all <- class_list_named |>
  imap(\(df, yr) {
    df |>
      mutate(
        year = as.integer(yr),                # 要素名を年度に
        prefecture = as.character(prefecture) # prefectureはchrに統一
      ) |>
      mutate(across(-prefecture, as.integer)) # それ以外はintegerに統一
  }) |>
  list_rbind()

#都道府県に全国共通の番号を付与
pref_order <- c(
  "北海道","青森","岩手","宮城","秋田","山形","福島",
  "茨城","栃木","群馬","埼玉","千葉","東京","神奈川",
  "新潟","富山","石川","福井","山梨","長野",
  "岐阜","静岡","愛知","三重",
  "滋賀","京都","大阪","兵庫","奈良","和歌山",
  "鳥取","島根","岡山","広島","山口",
  "徳島","香川","愛媛","高知",
  "福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄"
)

df_all <- df_all |>
  mutate(
    pref_id = map_int(prefecture, \(p) {
      # pref_order の中で部分一致する最初の位置を返す
      which(str_detect(p, pref_order))[1]
    })
  ) |>
  arrange(pref_id, year)

#61を含む列名を抽出(表記ゆれを直す)
cols_61 <- names(df_all)[str_detect(names(df_all), "61")]

#合算して over61 列を作り、元の列は削除
df_all <- df_all %>%
  mutate(over61 = rowSums(across(all_of(cols_61)), na.rm = TRUE)) %>%
  select(-all_of(cols_61)) %>%
  select(-over61) #over61列はすべての値が0だったので、のちの操作を見越して削除する

#long型へ
df_long <- df_all |>
  pivot_longer(
    cols = -c(year, prefecture, pref_id),  # 年・都道府県・id 以外を縦持ち
    names_to = "class_label",
    values_to = "school_count"
  ) |>
  mutate(
    class_num = if_else(
      str_detect(class_label, "～"),
      # 例: "25～30" → (25+30)/2
      map_dbl(class_label, \(x) {
        rng <- str_split(x, "～", simplify = TRUE)
        (as.numeric(rng[1]) + as.numeric(rng[2]))/2
      }),
      as.numeric(class_label)  # 普通の数字はそのまま
    ),
    # 学級数 × 学校数
    class_times_school = class_num * school_count
  )

df_summary <- df_long |>
  group_by(year, pref_id, prefecture) |>
  summarise(
    total_class_num = sum(class_times_school, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(pref_id, year)


pref_names <- c(
  "北海道","青森県","岩手県","宮城県","秋田県","山形県","福島県",
  "茨城県","栃木県","群馬県","埼玉県","千葉県","東京都","神奈川県",
  "新潟県","富山県","石川県","福井県","山梨県","長野県",
  "岐阜県","静岡県","愛知県","三重県",
  "滋賀県","京都府","大阪府","兵庫県","奈良県","和歌山県",
  "鳥取県","島根県","岡山県","広島県","山口県",
  "徳島県","香川県","愛媛県","高知県",
  "福岡県","佐賀県","長崎県","熊本県","大分県","宮崎県","鹿児島県","沖縄県"
)

df_summary <- df_summary |>
  mutate(prefecture_full = pref_names[pref_id]) |>
  relocate(prefecture_full, .after = pref_id) |>
  select(-pref_id, -prefecture) |>
  rename(prefecture = prefecture_full) 

saveRDS(df_summary, file = "data/cleaned/assign2.rds")

