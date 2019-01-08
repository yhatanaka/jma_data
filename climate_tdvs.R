# >> initial >>decl>>
#パッケージのインストール
# install.packages("tidyverse")

library("tidyverse")
library("magrittr")

# >>  1. データ読み込み >>decl>>
# >>decl
data_file = 'data-sakata-ymd-10-88-18.csv'

# >>  1. データ読み込み >>func>>
# >>func
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

# >> 1. データ読み込み >>proc>>
# >>proc
data_tbl <- read_jma(data_file)



# >>  2. ヘッダ header >>func>
# >>func
# ヘッダとして扱うのは何行目までか
# 先頭から"NA"が続く最後の行までが項目名。その後のちゃんと数字が始まる（NA じゃない）行からがデータ。
# ヘッダの行の最後は，as.numeric が NA 「ではない」（つまり数字）一番始めの（つまりデータの始まる）行の一つ前まで
header_row_end <- function(tbl) {
  result <- (tbl[,1] %>% purrr::map(as.numeric) %>% map({~ !is.na(.)}) %>% flatten_lgl %>% which %>% min)-1
  return(result)
}

# >> 2. ヘッダ header 最終行 >>proc>>
# >>proc
headers_end <- data_tbl %>% header_row_end

# >>  3. 年月日 ymd >>func>>
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
# >> 3. 年月日 ymd 最終列 >>proc>>
# >>proc
ymd_end <- data_tbl %>% ymd_col_end(ymd_col_names)

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

# >> 4. 年月日のヘッダから列名 >>proc>>
# >>proc
# 年月日のヘッダ
ymd_headers_tbl <- data_tbl %>% ymd_cols(ymd_end) %>% header_rows(headers_end)
# 1行目「集計開始」「集計終了」が入ってない列を穴埋め
# 一度転置してから下方向に補完して，元に戻す
ymd_headers_tbl[1,] %<>% t %>% as.data.frame() %>% tidyr::fill(1) %>% t
# 1・2行目変換後，くっつけて列名に
ymd_col_name <- apply(ymd_headers_tbl, 2, make_ymd_rowname, start_end_vec, ymd_vec)


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
  result1 <- rmv_str_1(dt[2])
  result2 <- rmv_str_2(dt[3])
  result_vec <- c(result1, result2)
  # result_vec から，NAではない要素を，"_"で連結して返す
  result <- paste(result_vec[!is.na(result_vec)], collapse="_")
  return(result)
}

# >> 5. 気象のヘッダから列名 >>proc>>
# >>proc
# 気象のヘッダ
clm_headers_tbl <- data_tbl %>% clm_cols(ymd_end) %>% header_rows(headers_end)
# 削除した2・3行目"くっつけて列名に
clm_col_name <- apply(clm_headers_tbl, 2, make_clm_rowname)



# >> 6. 1行目をベクトルに >>proc>>
# >>proc
first_row <- data_tbl %>% slice(1)
first_row_vec <- data_tbl %>% slice(1) %>% as.matrix() %>% as.vector()
# 気象データの列から地名
placeList <- first_row %>% clm_cols(ymd_end) %>% as.matrix() %>% as.vector() %>% unique

# >> 7. 最終的なデータを組み立てる >>proc>>
# >>proc
# 年月日の列名と("place"と)気象の列名つなげて，データに列名つける
# names(data_tbl) <- c(ymd_col_name, clm_col_name)
# データのヘッダ部分削除。地名の列 place 作る。開始年・月・日から日付の列 date 作る。date，年月日，地名，データの順に並べる
named_tbl <- data_tbl %>% data_rows(headers_end) %>% mutate(place = placeList[1]) %>% mutate(date = as.Date(str_c(.$X1,.$X2,.$X3, sep='/'))) %>% dplyr::select(date, c(1:ymd_end), place, which(grepl(placeList[1], first_row_vec)))

# start_m, start_d は 0 padding。
padding_date <- function(str){
  result <- str_sub(str_c('0',str), start = -2, end = -1)
  return(result)
}
named_tbl %<>% mutate(start_m = padding_date(start_m)) %>% mutate(start_d = padding_date(start_d))

# place, (項目名)_hoge を factor に，(項目名)を numeric に。
# 項目名のベクトル
named_colnames_vec <- colnames(named_tbl)
# 数値の列は，_ が無くて place でも無い列
data_col <- function(colnames_vec) {
  sapply(colnames_vec, function(x){str_detect(x, "^[^_]+$") && x != 'place'})
}
without_underbar_cols <- which(data_col(named_colnames_vec))
named_tbl_1 <- named_tbl %>% mutate(place = as.factor(place)) %>% mutate_at(vars(contains('_')), as.factor) %>% mutate_at(vars(without_underbar_cols), funs(as.numeric))

# 年ごとに日照時間，降水量を合計
year_sun_rain_tmpr <- named_tbl_1 %>% group_by_('start_y') %>% summarise(sun.total = sum(日照時間), rain.total = sum(降水量), tmpr.avr = mean(平均気温), tmpr.day_high = mean(日最高気温), tmpr.day_low = mean(日最低気温))
ggplot()+theme_set(theme_bw(base_size = 6,base_family="HiraKakuProN-W3"))
ggplot(data=year_sun_rain_tmpr) + geom_point(mapping = aes(x=start_y, y=rain.total))

# 月ごとに平均する。並べ替える関係で，start_m を factor から integer に変えてる。
month_sun_rain_tmpr <- named_tbl_1 %>% group_by_('start_y', 'start_m') %>% summarise(sun.total = sum(日照時間), rain.total = sum(降水量), tmpr.avr = mean(平均気温), tmpr.day_high = mean(日最高気温), tmpr.day_low = mean(日最低気温)) %>% mutate(start_m = as.integer(as.character(start_m))) %>% arrange(start_y, start_m)
comp_y_m <- function(y,m){
  y_str <- as.character(y)
  m_str <- as.character(m)
  m_str_2 <- padding_date(m_str)
  str_c(y_str,m_str_2,sep='/')
}
month_sun_rain_tmpr %<>% mutate(ym=comp_y_m(start_y,start_m)) %>% glimpse()


ggplot()+theme_set(theme_bw(base_size = 6,base_family="HiraKakuProN-W3"))
ggplot(data=month_sun_rain_tmpr) + geom_point(mapping = aes(x=ym, y=rain.total))

ggplot(data=named_tbl_1) + geom_point(mapping = aes(x=start_m, y=降水量))

ggplot(data=named_tbl_1) + geom_boxplot(mapping = aes(x=start_m, y=降水量))

ggplot(data=named_tbl_1, aes(x=start_m, y=降水量)) + geom_jitter(width=0.1) + geom_boxplot(alpha=0.3, outlier.shape = NA)

cCyan <- "#00a0e9"
cMagenta <- "#e4007f"
cGreen <- "#009944"
cOrange <- "#f39800"
cLightBlue <- "#0068b7"
qcolours <- c(cCyan,cMagenta,cGreen,cOrange,cLightBlue)

ggplot(data=named_tbl_1, aes(x=start_m, y=降水量)) + geom_boxplot(alpha=0.3, notch = TRUE, na.rm = TRUE, outlier.shape = NA, colour = '#00009966') + scale_fill_brewer(palette="Spectral") + geom_dotplot(binwidth=1, dotsize=3, binaxis="y", stackdir="center", na.rm = TRUE, colour = 'blue', alpha = 0.2)

ggplot(data=named_tbl_1, aes(x=start_m, y=降水量, colour='place')) + geom_violin(na.rm = TRUE, colour = '#00009966', fill = 'blue', alpha = 0.1) + geom_dotplot(binwidth=1, dotsize=4, binaxis="y", stackdir="center", na.rm = TRUE, colour = 'blue', alpha = 0.2) + geom_boxplot(alpha=0.4, notch = TRUE, na.rm = TRUE, outlier.shape = NA, colour = '#00009999')

