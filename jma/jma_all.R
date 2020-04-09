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


# >>  2. ヘッダ header >>func>
# >>func
# ヘッダとして扱うのは何行目までか
# 先頭から"NA"が続く最後の行までが項目名。その後のちゃんと数字が始まる（NA じゃない）行からがデータ。
# ヘッダの行の最後は，as.numeric が NA 「ではない」（つまり数字）一番始めの（つまりデータの始まる）行の一つ前まで
header_row_end <- function(tbl) {
  result <- (tbl[,1] %>% purrr::map(as.numeric) %>% map({~ !is.na(.)}) %>% flatten_lgl %>% which %>% min)-1
  return(result)
}

# >>  3. 年月日 ymd >>decl>>
ymd_col_names = c("集計開始", "集計終了")
# >>func
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


# >>func

# >>  4. 年月日のヘッダから列名 >>decl>>
# >>decl
# 1行目が「集計開始」=> start,「集計終了」=> end
start_end_vec <- c("集計開始" = "start", "集計終了" = "end")
# 2行目「年」=> y,「月」=> m,「日」=> d
ymd_vec <- c("年" = "y", "月" = "m", "日" = "d")

# >>  4. 年月日のヘッダから列名 >>func>>
# >>func
# ヘッダは hd_num 行まで
header_rows <- function(tbl, hd_num) {
  return(tbl %>% slice(c(1:hd_num)))
}
# それ以降，つまりデータの行
data_rows <- function(tbl, row_num) {
  return(tbl %>% slice(-c(1:row_num)))
}
# ymdの列
ymd_cols <- function(tbl, col_num) {
  return(tbl %>% dplyr::select(c(1:col_num)))
}
# それ以外の気象データの列
clm_cols <- function(tbl, col_num) {
  return(tbl %>% dplyr::select(-c(1:col_num)))
}

# 変換後，くっつけて列名に
make_ymd_rowname <- function(dt, vec1, vec2) {
  result1 <- vec1[dt[1]]
  result2 <- vec2[dt[2]]
  result <- paste(result1, result2, sep="_")
  return(result)
}


# >>  5. 気象のヘッダから列名 >>func>>
# >>func
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
  # result1 <- rmv_str_1(dt[2])
  result1 <- dt[2] %>% rmv_str_1
  # result2 <- rmv_str_2(dt[3])
  result2 <- dt[3] %>% rmv_str_2 %>% rmv_str_1
  result_vec <- c(result1, result2, dt[4] %>% rmv_str_2)
  # result_vec から，NAではない要素を，"_"で連結して返す
  result <- paste(result_vec[!is.na(result_vec)], collapse="_")
  return(result)
}


format_jun <- function(data_tbl){
  # >> 2. ヘッダ header 最終行 >>proc>>
  # >>proc
  headers_end <- data_tbl %>% header_row_end

  # >> 3. 年月日 ymd 最終列 >>proc>>
  # >>proc
  ymd_end <- data_tbl %>% ymd_col_end(ymd_col_names)

  # >> 4. 年月日のヘッダから列名 >>proc>>
  # >>proc
  # 年月日のヘッダ
  ymd_headers_tbl <- data_tbl %>% ymd_cols(ymd_end) %>% header_rows(headers_end)

  # 1行目「集計開始」「集計終了」が入ってない列を穴埋め
  # 一度転置してから下方向に補完して，元に戻す
  ymd_headers_tbl[1,] %<>% t %>% as.data.frame() %>% tidyr::fill(1) %>% t
  # 1・2行目変換後，くっつけて列名に
  ymd_col_name <- apply(ymd_headers_tbl, 2, make_ymd_rowname, start_end_vec, ymd_vec)

  # >> 5. 気象のヘッダから列名 >>proc>>
  # >>proc
  # 気象のヘッダ
  clm_headers_tbl <- data_tbl %>% clm_cols(ymd_end) %>% header_rows(headers_end)
  # 削除した2・3行目"くっつけて列名に
  clm_col_name <- apply(clm_headers_tbl, 2, make_clm_rowname)

  # >> 6. 1行目をベクトルに >>proc>>
  # >>proc
  place_row <- data_tbl %>% slice(1)
  place_row_vec <- place_row %>% as.matrix() %>% as.vector()
  # 気象データの列から地名
  placeList <- place_row %>% clm_cols(ymd_end) %>% as.matrix() %>% as.vector() %>% unique

  # >> 7. 最終的なデータを組み立てる >>proc>>
  # >>proc
  # 年月日の列名と("place"と)気象の列名つなげて，データに列名つける
  # データのヘッダ部分削除。地名の列 place 作る。開始年・月・日から日付の列 date 作る。date，年月日，地名，データの順に並べる
  # named_tbl <- data_tbl %>% data_rows(headers_end) %>% mutate(place = placeList[1]) %>% mutate(date = as.Date(str_c(.$X1,.$X2,.$X3, sep='/'))) %>% dplyr::select(date, c(1:ymd_end), place, which(grepl(placeList[1], place_row_vec)))

  # データのヘッダ部分削除
  databody_tbl <- data_tbl %>% data_rows(headers_end)

  # 年月日の部分 + 開始年・月・日から日付の列 st_date
  ymd_tbl <- databody_tbl %>% ymd_cols(ymd_end) %>% mutate_all(as.factor)
  ymd_tbl %<>% mutate(st_date = as.Date(str_c(.$X1,.$X2,.$X3, sep='/')))
  names(ymd_tbl) <- c(ymd_col_name,'st_date')
  # データヘッダ用
  data_colm_name_vec <- c(1:ymd_end, clm_col_name)


  # 地点ごとにデータまとめる
  place_data_list <- placeList %>% lapply(seperate_place_data, databody_tbl, place_row_vec, data_colm_name_vec)
  # 年月日の後に地点毎のデータくっつけて，最後に全地点のデータ一つにまとめる
  result <- place_data_list %>% lapply(cbind_r, ymd_tbl) %>% bind_rows()
  return(result)
}

# ある地点のデータ。地点のベクトル，データ，各列の地点名のベクトル，各列の列名ベクトル。 データからその地点のもの取り出し，列名付けて地名の列つけて返す。
seperate_place_data <- function(this_place, tbl, sel_plc_frm_here_vec, colm_name_vec){
  point_clm_tbl <- tbl %>% dplyr::select(which(grepl(this_place, sel_plc_frm_here_vec)))
  names(point_clm_tbl) <- c(colm_name_vec[which(grepl(this_place, sel_plc_frm_here_vec))])

  point_clm_tbl %<>% mutate(place = this_place)
  return(point_clm_tbl)
}

# 数値の列は，_ が無くて place でも無い列
with_underbar_col <- function(colnames_vec) {
  sapply(colnames_vec, function(x){str_detect(x, "^[^_]+$") && x != 'place'})
}

# 第1引数を後ろ側にくっつける
cbind_r <- function(data1,data2){
  return(cbind(data2,data1))
}


change_colm_type <- function(tbl){
  tbl_colnames_vec <- names(tbl)
  without_underbar_cols <- which(with_underbar_col(tbl_colnames_vec))
  tbl %<>% mutate_at(vars(contains('_')), funs(as.factor)) %>% mutate_at(vars(without_underbar_cols), funs(as.numeric)) %>% mutate(place = as.factor(place)) %>% mutate(st_date = as.Date(st_date))
  return(tbl)
}
