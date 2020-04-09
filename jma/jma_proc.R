data_folder <- './'


data_files <- list.files(data_folder, full.names = T, pattern = '.csv')
moddata_list <- data_files %>% lapply(read_jma)

# >> 2. ヘッダ header 最終行
headers_end <- data_tbl %>% header_row_end

# >> 3. 年月日 ymd 最終列
ymd_end <- data_tbl %>% ymd_col_end(ymd_col_names)

# >> 4. 年月日のヘッダから列名
# 年月日のヘッダ
ymd_headers_tbl <- data_tbl %>% ymd_cols(ymd_end) %>% header_rows(headers_end)
# 1行目「集計開始」「集計終了」が入ってない列を穴埋め
# 一度転置してから下方向に補完して，元に戻す
ymd_headers_tbl[1,] %<>% t %>% as.data.frame() %>% tidyr::fill(1) %>% t
# 1・2行目変換後，くっつけて列名に
ymd_col_name <- apply(ymd_headers_tbl, 2, make_ymd_rowname, start_end_vec, ymd_vec)

# >> 5. 気象のヘッダから列名
# 気象のヘッダ
clm_headers_tbl <- data_tbl %>% clm_cols(ymd_end) %>% header_rows(headers_end)
# 削除した2・3行目"くっつけて列名に
clm_col_name <- apply(clm_headers_tbl, 2, make_clm_rowname)

# >> 6. 1行目をベクトルに
first_row <- data_tbl %>% slice(1)
first_row_vec <- data_tbl %>% slice(1) %>% as.matrix() %>% as.vector()
# 気象データの列から地名
placeList <- first_row %>% clm_cols(ymd_end) %>% as.matrix() %>% as.vector() %>% unique

# >> 7. 最終的なデータを組み立てる
# 年月日の列名と("place"と)気象の列名つなげて，データに列名つける
# names(data_tbl) <- c(ymd_col_name, clm_col_name)
# データのヘッダ部分削除。地名の列 place 作る。開始年・月・日から日付の列 date 作る。date，年月日，地名，データの順に並べる
named_tbl <- data_tbl %>% data_rows(headers_end) %>% mutate(place = placeList[1]) %>% mutate(st_date = as.Date(str_c(.$X1,.$X2,.$X3, sep='/'))) %>% dplyr::select(st_date, c(1:ymd_end), place, which(grepl(placeList[1], first_row_vec)))
# 年月日の列名と("place"と)気象の列名つなげて，データに列名つける
names(named_tbl) <- c("st_date", ymd_col_name, "place", clm_col_name)

named_tbl %<>% mutate(start_m = padding_date(start_m)) %>% mutate(start_d = padding_date(start_d))

# place, (項目名)_hoge を factor に，(項目名)を numeric に。
# 項目名のベクトル
named_colnames_vec <- colnames(named_tbl)
without_underbar_cols <- which(data_col(named_colnames_vec))
named_tbl_1 <- named_tbl %>% mutate(place = as.factor(place)) %>% mutate_at(vars(contains('_')), as.factor) %>% mutate_at(vars(without_underbar_cols), funs(as.numeric))
