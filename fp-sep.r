# >> 1. データ読み込み >>func>>
read_jma <- function(csv_f) {
  f <-
    read_csv(
      data_file
      , col_names = FALSE
      , skip = 1
      , locale = locale(encoding = "cp932")
    )
  # 空行削除
  return(f %>% filter(rowSums(is.na(.)) != ncol(.)))
}

# >> 2. ヘッダ header >>func>>
# ヘッダとして扱うのは何行目までか
# 先頭から"NA"が続く最後の行までが項目名。その後のちゃんと数字が始まる（NA じゃない）行からがデータ。
# ヘッダの行の最後は，as.numeric が NA 「ではない」（つまり数字）一番始めの（つまりデータの始まる）行の一つ前まで
header_row_end <- function(tbl) {
  result <- (tbl[,1] %>% purrr::map(as.numeric) %>% map({~ !is.na(.)}) %>% flatten_lgl %>% which %>% min)-1
  return(result)
}
# ヘッダは hd_num 行まで
header_rows <- function(tbl, hd_num) {
  return(tbl %>% slice(c(1:hd_num)))
}
# それ以降，つまりデータの行
data_rows <- function(tbl, row_num) {
  return(tbl %>% slice(-c(1:row_num)))
}

# >> 3. 年月日 ymd >>func>>
# 年月日を表す列はどこまでか
# 先頭で宣言した ymd_col_names や NA「以外」（つまり地名）かどうか
not_ymd_col <- function(x, col_names) {
  result <- !(x %in% col_names) && !is.na(x)
  return(result)
}
# 年月日の列の最後は，地名が始まる一番最初の列(not_ymd_col が TRUE になる最初の列)の，1つ前
ymd_col_end <- function(tbl, col_names) {
  (sapply(tbl[1,], not_ymd_col, col_names) %>% which %>% min) -1
}
# ymdの列
ymd_cols <- function(tbl, col_num) {
  return(tbl %>% dplyr::select(c(1:col_num)))
}
# それ以外の気象データの列
clm_cols <- function(tbl, col_num) {
  return(tbl %>% dplyr::select(-c(1:col_num)))
}


# >> 4. 年月日のヘッダから列名 >>func>>
# 変換後，くっつけて列名に
make_ymd_rowname <- function(dt, vec1, vec2) {
  result1 <- vec1[dt[1]]
  result2 <- vec2[dt[2]]
  result <- paste(result1, result2, sep="_")
  return(result)
}

# >> 5. 気象のヘッダから列名 >>func>>
# 2行目の「(時間)」「(℃)」「(mm)」「の平均」「の合計」削除
rmv_str_1 <- function(str) {
  result <- str %>% sub("の.+", "", .) %>% sub("\\(.+", "", .)
  return(result)
}
# 3行目の「番号」「情報」削除
rmv_str_2 <- function(str) {
  result <- str %>% sub("情報", "", .) %>% sub("番号", "", .)
  return(result)
}
# 削除した2・3行目"くっつけて列名に
make_clm_rowname <- function(dt) {
  result1 <- rmv_str_1(dt[2])
  result2 <- rmv_str_2(dt[3])
  result_vec <- c(result1, result2)
  # result_vec から，NAではない要素を，"_"で連結して返す
  result <- paste(result_vec[!is.na(result_vec)], collapse="_")
  return(result)
}


# ************

# >> 1. データ読み込み >>proc>>
data_tbl <- read_jma(data_file)

# >> 2. ヘッダ header 最終行 >>proc>>
headers_end <- data_tbl %>% header_row_end
# >> 3. 年月日 ymd 最終列 >>proc>>
ymd_end <- data_tbl %>% ymd_col_end(ymd_col_names)

# data_tbl %>% ymd_cols(ymd_end) %>% header_rows(headers_end)
# data_tbl %>% clm_cols(ymd_end) %>% header_rows(headers_end)
# data_tbl %>% ymd_cols(ymd_end) %>% data_rows(headers_end)
# data_tbl %>% clm_cols(ymd_end) %>% data_rows(headers_end)

# >> 4. 年月日のヘッダから列名 >>proc>>
# 年月日のヘッダ
ymd_headers_tbl <- data_tbl %>% ymd_cols(ymd_end) %>% header_rows(headers_end)
# 1行目「集計開始」「集計終了」が入ってない列を穴埋め
# 一度転置してから下方向に補完して，元に戻す
ymd_headers_tbl[1,] %<>% t %>% as.data.frame() %>% tidyr::fill(1) %>% t
# 1・2行目変換後，くっつけて列名に
ymd_col_name <- apply(ymd_headers_tbl, 2, make_ymd_rowname, start_end_vec, ymd_vec)

# >> 5. 気象のヘッダから列名 >>proc>>
# 気象のヘッダ
clm_headers_tbl <- data_tbl %>% clm_cols(ymd_end) %>% header_rows(headers_end)
# 削除した2・3行目"くっつけて列名に
clm_col_name <- apply(clm_headers_tbl, 2, make_clm_rowname)

# >> 6. 1行目をベクトルに >>proc>>
first_row <- data_tbl %>% slice(1)
first_row_vec <- data_tbl %>% slice(1) %>% as.matrix() %>% as.vector()
# 気象データの列から地名
placeList <- first_row %>% clm_cols(ymd_end) %>% as.matrix() %>% as.vector() %>% unique

# >> 7. 最終的なデータを組み立てる >>proc>>
# データのヘッダ部分とって，地名の列作り，年月日，地名，データの順に並べる
data_tbl %<>% data_rows(headers_end) %>% mutate(place = placeList[1]) %>% dplyr::select(c(1:ymd_end), place, which(grepl(placeList[1], first_row_vec)))

# 年月日の列名と("place"と)気象の列名つなげて，データに列名つける
names(data_tbl) <- c(ymd_col_name, "place", clm_col_name)

