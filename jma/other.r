# >>  3. 年月日 ymd >>decl>>
ymd_col_names = c("集計開始", "集計終了")

# >>  3. 年月日 ymd >>func>>
# 年月日を表す列はどこまでか
# 先頭で宣言した ymd_col_names や NA「以外」（つまり地名）かどうか
notYmdCol <- function(x, col_names) {
  result <- !(x %in% col_names) && !is.na(x)
  return(result)
}

# >>  4. 年月日のヘッダから列名 >>decl>>
# >>decl
# 1行目が「集計開始」=> start,「集計終了」=> end
start_end_vec <- c("NA" = "start", "集計開始" = "start", "集計終了" = "end")

# 変換後，くっつけて列名に
makeYmdRowname <- function(dt) {
  if(!is.na(dt[1])) {
    result1 <- start_end_vec[dt[1]]
  } else {
    result1 <- start_end_vec["NA"]
  }
  result2 <- ymd_vec[dt[2]]
  result <- paste(result1, result2, sep="_")
  return(result)
}


formatJun <- function(data_tbl){
   # 1行目「集計開始」「集計終了」が入ってない列を穴埋め
  # 一度転置してから下方向に補完して，元に戻す
  ymd_headers_tbl[1,] %<>% t %>% as.data.frame() %>% tidyr::fill(1) %>% t
  # 1・2行目変換後，くっつけて列名に
  ymd_col_name <- apply(ymd_headers_tbl, 2, makeYmdRowname)
}


# >> 7. 最終的なデータを組み立てる
# start_m, start_d は 0 padding。
paddingDate <- function(str){
  result <- str_sub(str_c('0',str), start = -2, end = -1)
  return(result)
}
