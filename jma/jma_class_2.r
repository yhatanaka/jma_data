library("tidyverse")
library("magrittr")
library("R6")

# install.packages('tidyverse')
# install.packages('magrittr')
# install.packages('R6')


Jma <- R6Class(
  classname = "Jma",
  public = list(
    data_path = NA
    ,
    data_tbl_list = NA
    ,
    # headers_end = NA
    # ,
    # ymd_end = NA
    # ,
    # ymd_headers_tbl = NA
    # ,
    ymd_col_name = NA
    ,
    # data_headers_tbl = NA
    # ,
    # data_col_name = NA
    # ,
    # placeList = NA
    # ,
    # compositedData = NA
    # ,
    initialize = function(data_path) {
      self$data_path <- data_path
      self$data_tbl_list <- private$readJma(self$data_path)
    }
    ,
    # >> 4. 年月日のヘッダから列名　最終データ組み立てで使用
    # 年月日のヘッダ
    ymdColName = function(data_tbl) {
      # >> 2. ヘッダ header 最終行
      headers_end <- private$headerRowEnd(data_tbl)
      # >> 3. 年月日 ymd 最終列
      ymd_end <- private$ymdColEnd2(data_tbl)

      ymd_headers_tbl <- data_tbl %>% private$ymdCols(ymd_end) %>% private$headerRows(headers_end)
      # 1・2行目変換後，くっつけて列名に
      ymd_col_name <- apply(ymd_headers_tbl, 2, private$makeYmdClmName)
      return(ymd_col_name)
    }
    ,
    # >> 5. 気象データのヘッダから列名　最終データ組み立てで使用
    # 気象データのヘッダ
    dataColName = function(data_tbl) {
      # >> 2. ヘッダ header 最終行
      headers_end <- private$headerRowEnd(data_tbl)
      # >> 3. 年月日 ymd 最終列
      ymd_end <- private$ymdColEnd(data_tbl)

      data_headers_tbl <- data_tbl %>% private$dataCols(ymd_end) %>% private$headerRows(headers_end)
      # 削除した2・3行目"くっつけて列名に
      data_col_name <- apply(data_headers_tbl, 2, private$makeDataClmName)
      return(data_col_name)
    }
    ,
    # >> 7. 最終的なデータを組み立てる 1
    # 年月日の列名と("place"と)気象の列名つなげて，データに列名つける
    # データのヘッダ部分削除。地名の列 place 作る。開始年・月・日から日付の列 date 作る。date，年月日，地名，データの順に並べる
    compositData = function(data_tbl) {
      # >> 2. ヘッダ header 最終行
      headers_end <- private$headerRowEnd(data_tbl)
      # >> 3. 年月日 ymd 最終列
      ymd_end <- private$ymdColEnd(data_tbl)
      # >> 3. 開始日 最終列
      ymd_end2 <- private$ymdColEnd2(data_tbl)
      # データのヘッダ部分削除
      databody_tbl <- data_tbl %>% private$dataRows(headers_end)
      # 年月日の部分 + 開始年・月・日から日付の列 st_date
      # ymd_tbl <- databody_tbl %>% private$ymdCols(ymd_end2) %>% mutate_all(as.factor)
      ymd_tbl <- databody_tbl %>% private$ymdCols(ymd_end2)
      if (ncol(ymd_tbl) == 2) {
        ymd_tbl %<>% mutate(st_date = str_c(.$X1, .$X2, sep = '/'))
      } else if (ncol(ymd_tbl) >= 3) {
        ymd_tbl %<>% mutate(st_date = as.Date(str_c(.$X1, .$X2, .$X3, sep = '/')))
      }
      self$ymd_col_name = self$ymdColName(data_tbl)
      names(ymd_tbl) <- c(self$ymd_col_name, 'st_date')

      # データヘッダ用
      data_col_name = self$dataColName(data_tbl)
      # 地点ごとにデータまとめる
      # placeList_vec <- private$makePlaceList(data_tbl %>% private$dataCols(ymd_end), ymd_end)
      placeList_vec <- private$makePlaceList(data_tbl, ymd_end)
      placeList <- placeList_vec[['placeList']]
      place_row_vec <- placeList_vec[['place_row_vec']]
      place_data_list <- placeList %>% lapply(private$seperatePlaceData,
                             databody_tbl %>% private$dataCols(ymd_end),
                             place_row_vec,
                             data_col_name)
      # 年月日の後に地点毎のデータくっつけて，最後に全地点のデータ一つにまとめる
      compositedData <- lapply(place_data_list, function(x){cbind(ymd_tbl, x)})
      return(compositedData)
    }
    ,
    compositData_test = function(data_tbl) {
      # >> 2. ヘッダ header 最終行
      headers_end <- private$headerRowEnd(data_tbl)
      # >> 3. 年月日 ymd 最終列
      ymd_end <- private$ymdColEnd(data_tbl)
      # >> 3. 開始日 最終列
      ymd_end2 <- private$ymdColEnd2(data_tbl)
      # データのヘッダ部分削除
      databody_tbl <- data_tbl %>% private$dataRows(headers_end)
      # 年月日の部分 + 開始年・月・日から日付の列 st_date
      ymd_tbl <- databody_tbl %>% private$ymdCols(ymd_end2) %>% mutate_all(as.factor)
      if (ncol(ymd_tbl) == 1) {
        # 日付リテラル(y/m/d) start_NA
        ymd_list <- ymd_tbl$X1 %>% str_split('/')
        ymd_tbl %<>% mutate(st_date = .$X1) %>% mutate(start_y = ymd_list[[1]][1]) %>% mutate(start_m = ymd_list[[1]][2])
        if (ymd_list %>% length() == 3) {
          ymd_tbl %>% mutate(start_d = ymd_list[[1]][3])
        }
      } else if (ncol(ymd_tbl) == 2) {
        ymd_tbl %<>% mutate(st_date = str_c(.$X1, .$X2, sep = '/'))
      } else if (ncol(ymd_tbl) == 3) {
        ymd_tbl %<>% mutate(st_date = as.Date(str_c(.$X1, .$X2, .$X3, sep = '/')))
      }
      self$ymd_col_name = self$ymdColName(data_tbl)
      names(ymd_tbl) <- c(self$ymd_col_name, 'st_date')

      # データヘッダ用
      data_col_name = self$dataColName(data_tbl)
      # 地点ごとにデータまとめる
      # placeList_vec <- private$makePlaceList(data_tbl %>% private$dataCols(ymd_end), ymd_end)
      placeList_vec <- private$makePlaceList(data_tbl, ymd_end)
      placeList <- placeList_vec[['placeList']]
      place_row_vec <- placeList_vec[['place_row_vec']]
      place_data_list <- placeList %>% lapply(private$seperatePlaceData,
                                              databody_tbl %>% private$dataCols(ymd_end),
                                              place_row_vec,
                                              data_col_name)

      return(self$data_tbl_list)
    }
    ,
    allData = function() {
      result <- lapply(self$data_tbl_list, self$compositData) %>% bind_rows()

      # dataColm_name_vec <- c(1:ymd_end, data_col_name), data -> (_無し)dbl (_)factor, place -> factor
      # ymdColName(1:ymd_end) -> factor, st_date -> Date,

      tbl_colnames_vec <- names(result)
      without_underbar_cols <- which(private$withUnderbarCol(tbl_colnames_vec))
      # result %<>% mutate_at(vars(contains('_')), list(~as.factor(.))) %>% mutate_at(vars(all_of(without_underbar_cols)), list(~as.numeric(.))) %>% mutate(place = as.factor(place)) %>% mutate(st_date = as.Date(st_date))
      # result %<>% mutate_at(self$ymd_col_name %>% as.vector, list(~as.factor(.)))
      return(result)
    }
    ,
    allData_test = function() {
      # result <- lapply(self$data_tbl_list, self$compositData) %>% bind_rows()

      result <- lapply(self$data_tbl_list, self$compositData_test)
      return(result)

      # dataColm_name_vec <- c(1:ymd_end, data_col_name), data -> (_無し)dbl (_)factor, place -> factor
      # ymdColName(1:ymd_end) -> factor, st_date -> Date,

      # tbl_colnames_vec <- names(result)
      # without_underbar_cols <- which(private$withUnderbarCol(tbl_colnames_vec))
      # result %<>% mutate_at(vars(contains('_')), list(~as.factor(.))) %>% mutate_at(vars(all_of(without_underbar_cols)), list(~as.numeric(.))) %>% mutate(place = as.factor(place)) %>% mutate(st_date = as.Date(st_date))
      # result %<>% mutate_at(self$ymd_col_name %>% as.vector, list(~as.factor(.)))
      # return(result)
    }
    ,
    test = function(data_tbl) {
      headers_end <- private$headerRowEnd(data_tbl)
      ymd_end2 <- private$ymdColEnd2(data_tbl)
      databody_tbl <- data_tbl %>% private$dataRows(headers_end)
      ymd_tbl <- databody_tbl %>% private$ymdCols(ymd_end2) %>% mutate_all(as.factor)
      ymd_tbl %<>% mutate(st_date = as.Date(str_c(.$X1, .$X2, .$X3, sep = '/')))
      self$ymd_col_name = self$ymdColName(data_tbl)
      names(ymd_tbl) <- c(self$ymd_col_name, 'st_date')
      data_col_name = self$dataColName(data_tbl)
      placeList_vec <- private$makePlaceList(data_tbl, ymd_end2)
      placeList <- placeList_vec[['placeList']]
      place_row_vec <- placeList_vec[['place_row_vec']]
      place_data_list <- placeList %>% lapply(private$seperatePlaceData,
                                              databody_tbl %>% private$dataCols(ymd_end2),
                                              place_row_vec,
                                              data_col_name)

      return(place_data_list)
    }

  )
  ,
  private = list(
    readJma = function(data_path) {
      # csvファイル
      if ((data_path %>% str_sub(start = -4,end = -1)) == ".csv") {
        return(list(private$readDataFile(data_path)))
      } else if ((data_path %>% str_sub(start = -1,end = -1)) == "/") {
        return(private$dataInThisFolder(data_path))
      }
    }
    ,
    readDataFile = function(csv_f) {
      f <-
        read_csv(
          csv_f
          ,
          col_names = FALSE
          ,
          skip = 1
          ,
          skip_empty_rows = TRUE
          ,
          n_max = Inf
          ,
# 余計な変換（2000行超えると、タイプの違うデータ読み飛ばしたりする。例えば、ヘッダに相当する「年」「月」「日」）
          col_types = cols(.default = "c")
          ,
          locale =  readr::locale(encoding = "cp932")
        )
      # 空行削除
      # return(f %>% filter(rowSums(is.na(.)) != ncol(.)))

    }
    ,
    dataInThisFolder = function(data_folder) {
      data_files <- list.files(data_folder, full.names = T, pattern = '.csv')
      moddata_list <- data_files %>% lapply(private$readDataFile)
      return(moddata_list)
    }
    ,
    place_row_vec = NA
    ,
    # >>  2. ヘッダ header >>func>
    # ヘッダとして扱うのは何行目までか
    # 先頭から"NA"が続く最後の行までが項目名。その後のちゃんと数字が始まる（NA じゃない）行からがデータ。
    # ヘッダの行の最後は，as.numeric が NA 「ではない」（つまり数字）一番始めの（つまりデータの始まる）行の一つ前まで
    headerRowEnd = function(data_tbl) {
# 年月日が"YYYY/MM/DD"形式の場合に備えて、先頭2列ははずす
      result <-
        (data_tbl[, 3] %>% purrr::map(as.numeric) %>% map({~ !is.na(.)}) %>% flatten_lgl %>% which %>% min) - 1
      return(result)
    }
    ,
    # >>  3. 年月日 ymd >>func>>
    # 年月日を表す列はどこまでか
    # "集計開始", "集計終了", NA「以外」（つまり地名）かどうか
    notYmdCol = function(x) {
      result <- !(x %in% c("集計開始", "集計終了")) && !is.na(x)
      return(result)
    }
    ,
    # 開始日を表す列はどこまでか
    #  "集計開始", NA「以外」（つまり「集計終了」か地名）かどうか
    notYmdCol2 = function(x) {
      result <- !(x %in% c("集計開始")) && !is.na(x)
      return(result)
    }
    ,
    # 年月日の列の最後は，地名が始まる一番最初の列(notYmdCol が TRUE になる最初の列)の，1つ前
    ymdColEnd = function(data_tbl) {
      (sapply(data_tbl[1, ], private$notYmdCol) %>% which %>% min) - 1
    }
    ,
    # 開始日の列の最後は，「集計終了」 or 地名が始まる一番最初の列(notYmdCol が TRUE になる最初の列)の，1つ前
    ymdColEnd2 = function(data_tbl) {
      (sapply(data_tbl[1, ], private$notYmdCol2) %>% which %>% min) - 1
    }
    ,
    # >>  4. 年月日のヘッダから列名 >>decl>>
    # 2行目「年」=> y,「月」=> m,「日」=> d
    ymd_vec = c("年" = "y", "月" = "m", "日" = "d", "時" = "h")
    ,
    # >>  4. 年月日のヘッダから列名
    # ヘッダは hd_num 行まで
    headerRows = function(tbl, hd_num) {
      return(tbl %>% slice(c(1:hd_num)))
    }
    ,
    # それ以降，つまりデータの行
    dataRows = function(tbl, row_num) {
      return(tbl %>% slice(-c(1:row_num)))
    }
    ,
    # ymdの列
    ymdCols = function(tbl, col_num) {
      return(tbl %>% dplyr::select(c(1:all_of(col_num))))
    }
    ,
    # それ以外の気象データの列
    dataCols = function(tbl, col_num) {
      return(tbl %>% dplyr::select(-c(1:all_of(col_num))))
    }
    ,
    # 年月日のヘッダ，変換後，くっつけて列名に
    makeYmdClmName = function(dt) {
      result <- private$ymd_vec[dt[2]]
      return(paste('start', result, sep="_"))
    }
    ,
    # >>  5. 気象のヘッダから列名
    # 2行目の「(時間)」「(℃)」「(mm)」「の平均」「の合計」削除
    rmvStr_1 = function(str) {
      result <- str %>% str_replace("の.+", "") %>% str_replace("\\(.+", "")
      return(result)
    }
    ,
    # 3行目の「番号」「情報」削除
    rmvStr_2 = function(str) {
      result <- str %>% str_replace("情報", "") %>% str_replace("番号", "")
      return(result)
    }
    ,
    # データのヘッダ，削除した2・3行目"くっつけて列名に
    makeDataClmName = function(dt) {
      result1 <- dt[2] %>% private$rmvStr_1()
      result2 <- dt[3] %>% private$rmvStr_2() %>% private$rmvStr_1()
      result_vec <- c(result1, result2, dt[4] %>% private$rmvStr_2())
      # result_vec から，NAではない要素を，"_"で連結して返す
      result <- paste(result_vec[!is.na(result_vec)], collapse = "_")
      return(result)
    }
    ,
    # 気象データの列から地名　最終データ組立てで使用
    makePlaceList = function(data_tbl, ymd_end) {
      # >> 6. 1行目をベクトルに
      place_row <- data_tbl %>% private$dataCols(ymd_end) %>% slice(1)
      place_row_vec <- place_row %>% as.matrix() %>% as.vector()
      # 気象データの列から地名
      placeList <- place_row %>% as.matrix() %>% as.vector() %>% unique
      # return(placeList)
      return(list('placeList' = placeList, 'place_row_vec' = place_row_vec))
    }
    ,
    # ある地点のデータ。地点のベクトル，データ，各列の地点名のベクトル，各列の列名ベクトル。 データからその地点のもの取り出し，列名付けて地名の列つけて返す。
    seperatePlaceData = function(this_place, tbl, sel_plc_frm_here_vec, colm_name_vec){
      # データ列の中から，ある地名の列だけ取り出す
      point_clm_tbl <- tbl %>% dplyr::select(which(grepl(this_place, sel_plc_frm_here_vec)))
      names(point_clm_tbl) <- c(colm_name_vec[which(grepl(this_place, sel_plc_frm_here_vec))])
      # 最後に'place'列付け加える
      point_clm_tbl %<>% mutate(place = this_place)
      return(point_clm_tbl)
    }
    ,
    # 数値の列は，_ が無くて place でも無い列
    withUnderbarCol = function(colnames_vec) {
      sapply(colnames_vec, function(x){str_detect(x, "^[^_]+$") && x != 'place'})
    }


  )
)
