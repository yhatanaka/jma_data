# 項目ごとに分ける
# まずは list(地名->df, 地名->df)
# データ読み込み ファイル名指定
f <-
  read.table(
    'data-m.csv',
    sep = ",",
    skip = 1,
    header = F,
    fileEncoding = "Shift_JIS",
    stringsAsFactors = F,
# 空白を削除する
        na.strings=c("", "NULL")
)

# # 空行削除
is_blank <- function(x){
  is.na(x) | x == ""
}
# すべてが空欄である行を探す
unnecessary_row <- apply(f, 1, function(x){
  all(is_blank(x))
})
# 「すべてが空欄である行」以外を残す（＝空行の除去）
f <- f[!unnecessary_row,]


# # 地点ごとに分割
# 場所のリスト
placeWithNull <- unique(as.vector(as.matrix(f[1,])[1,]))
placeList <- placeWithNull[!is.na(placeWithNull)]

# 地点ごとに分離してリストに格納
dfList <- list()
for(i in placeList){
# 先頭列は日付
  yearMonthDf <- data.frame(as.character(f[,1]))
  thisPlaceDf <- f[,which(f[1,] == i)]
  thisPlaceDf2 <- cbind(yearMonthDf, thisPlaceDf)
  dfList[[i]] <- thisPlaceDf2
}


# 各地点のデータフレーム加工
# 2行目_3行目
removeCharFromTitle1 <- function(string) {
  return(gsub("(\\(.+\\))|(の.+)", "", string))
}
removeCharFromTitle2 <- function(string) {
  return(gsub("(情報)|(番号)", "", string))
}
makeColumnName <- function(dfCol, titleColumnArray) {
  retCol = removeCharFromTitle1(dfCol[titleColumnArray])
  if (!is.na(dfCol[titleColumnArray+1])) {
    retCol = paste(retCol, removeCharFromTitle2(dfCol[titleColumnArray+1]), sep = "_")
  }
  return(retCol)
}
# タイトルに設定
setCombinedColumn <- function(df){
  colnames(df) <- apply(df, 2, makeColumnName, 2)
  return(df)
}
dfList2 <- lapply(dfList, setCombinedColumn)

removeUnnesecRow <- function(df, removeColumnArray){
  # 1(地名),2(平均気温だの),3(均質情報だの)行目削除
  df <- df[removeColumnArray, ]
}
dfList3 <- lapply(dfList2, removeUnnesecRow, c(-1:-3))


# 「年月」を日付に。
changeToDate <- function(df){
  # 「年/月」を，その月の1日の日付型に。
  df[, 1] <- as.Date(paste(df[, 1], "/1", sep = ""))
  return(df)
}

dfList4 <- lapply(dfList3, changeToDate)

# "年/月" の月の部分，"month"の列にfactor型で
# まず月の部分取り出す
monthLabel <- function(x) {
  return(format(x, "%m"))
}

makeMonth <- function(df){
  df$month <- as.factor(sapply(df$年月, monthLabel))
  return(df)
}

dfList5 <- lapply(dfList4, makeMonth)

# 「_」でつないだとこ（品質情報，均質番号，現象なし）はfactor型に
# 「_」でつないだ列を返す関数作ってとりあえず泥臭く for で回す
combinedColumn <- function(df) {
  return(grepl(".+_.+", colnames(df)))
}

itemAppendixToFactor <- function(df){
  for (i in which(combinedColumn(df))) {
    df[, i] <- as.factor(df[, i])
  }
  return(df)
}

dfList6 <- lapply(dfList5, itemAppendixToFactor)

# test
# testFunc <- function(dfColumn,isStartWith_){
#   if(isStartWith_){
#     return(as.factor(dfColumn))
#   } else {
#     return(dfColumn)
#   }
# }
# itemAppendixToFactor_2 <- function(df){
#   testDf <- as.data.frame(mapply(testFunc,df,combinedColumn(df), SIMPLIFY=F))
#   return(testDf)
# }
# df_test <- lapply(dfList5, itemAppendixToFactor_2)
# str(df_test[["酒田"]])


# 「平均気温」「日最高気温」「降水量」などのデータ部分は numeric型に変換
# (colnames が「年月」「month」以外で，しかも「_」ない)の列
dataColumn <- function(df) {
  dataFlag1 = !grepl("(.+_.+)", colnames(df))
  dataFlag2 = !grepl("(年月)|(month)", colnames(df))
  return(as.logical(dataFlag1 * dataFlag2))
}

# 項目名をnumeric(数値)型に
itemNameToNumeric <- function(df){
  for (i in which(dataColumn(df))) {
    df[, i] <- as.numeric(df[, i])
  }
return(df)
}

dfList7 <- lapply(dfList6, itemNameToNumeric)

# ついで
# 項目名
getItemName <- function(df){
  colnames(df)[dataColumn(df)]
}
itemName <- lapply(dfList7, getItemName)
# ついでに年月の範囲
getDateRange <- function(df){
  paste(format(min(df$年月), "%Y/%m"), format(max(df$年月), "%Y/%m"), sep = " 〜 ")
}
dateRange <- lapply(dfList7, getDateRange)
#



paddingString <- function(numVec) {
  addingMonthNum <- (max(numVec) + 1):12
  # 0 paddingした文字列に変換したvector
  addingMonthString <- sprintf("%02d", addingMonthNum)
  return(addingMonthString)
}

# まだ来てない月の分，項目としては作っておく
notYetMonthVec <- function(yearDF) {
  itemDF <- yearDF[, c("month")]
  # 12月までない場合に，付け足す分のvectorを返す
  if (length(itemDF) < 12) {
    itemDFWithNumMonth <- as.numeric(itemDF)
    notYetMonthStringVec <- paddingString(itemDFWithNumMonth)
    return(notYetMonthStringVec)
  } else {
    return(c())
  }
}

# 特定の項目のデータ
pickData <- function(df, itemName) {
  return(df[, c("month", itemName)])
}

# 付け足す分
addRestMonth <- function(monthDataDF, paddinfStringsVec) {
  for (i in paddinfStringsVec) {
    monthDataDF <- rbind(monthDataDF, c(i, ""))
  }
  return(monthDataDF)
}

plotYear <- function(yearDF, itemName) {
  # その年の，ある項目のデータ
  yearDFForPlotTmp <- pickData(yearDF, itemName)
  # 今年だと，まだ来てない月のデータがない。その分の月を追加してグラフの描画に合わせる
  yearDFForPlot <-
    addRestMonth(yearDFForPlotTmp, notYetMonthVec(yearDF))
  return(yearDFForPlot)
}

# 特定の年のデータ
getYearDF <- function(jmaDF, pickedYear) {
  yearDF <- subset(jmaDF, grepl(pickedYear, format(jmaDF$年月, "%Y")))
  #str(yearDF)
  return(yearDF)
}

getSDBar <- function(jmaDF, itemName) {
  avrByMonth <- tapply(jmaDF[, c(itemName)], jmaDF$month[!is.na(jmaDF$month)], mean, na.rm=T)
  sdByMonth <- tapply(jmaDF[, c(itemName)], jmaDF$month[!is.na(jmaDF$month)], sd, na.rm=T)
  resultSDVec <- list(avrByMonth, sdByMonth)
  names(resultSDVec) <- c("avr", "sd")
  return(resultSDVec)
}

# プロット用データ データフレーム と年と項目名
plotDotLine <- function(df, year, itemName) {
  # ある年のある項目のデータ
  resultDF <- plotYear(getYearDF(df, year), itemName)
  # その項目の，今までの平均値と標準偏差
  resultSD <- getSDBar(df, itemName)
  # その年の，項目の数字の最大値・最小値(欠損値除く)
  yDataMin <- min(as.numeric(resultDF[, 2]), na.rm = T)
  yDataMax <- max(as.numeric(resultDF[, 2]), na.rm = T)
  # 平均値+標準偏差の最大値
  ySDMin <- min(resultSD$avr - resultSD$sd, na.rm = T)
  # 平均値-標準偏差の最小値
  ySDMax <- max(resultSD$avr + resultSD$sd, na.rm = T)

  # その年のと平均値〜と，どっちか大きい/小さい方(ただし最小値は0含む)
  yMin <- min(yDataMin, ySDMin, 0)
  yMax <- max(yDataMax, ySDMax)
  resultVec <- list(data=resultDF[,2], avr=resultSD$avr, sd=resultSD$sd, ylim=c(yMin, yMax), axtxt=paste(itemName, year))
  return(resultVec)
}

# example 〜〜年の「〜〜」のデータと，今までの標準偏差プロット
# まずデータフレーム と年と項目名
pldata <- plotDotLine(dfList7[["酒田"]], "2018", "降水量")
ydata <- pldata$data
allAvr <- pldata$avr
allSD <- pldata$sd
allylim <- pldata$ylim
axtxt <- pldata$axtxt

allData <- plotYear(dfList7[["酒田"]], "降水量")
allData[,1] <- as.numeric(as.character(allData[,1]))
yMax <- max(allData[,2], na.rm=T)

plot(allData, pch=1, xlim=c(1,12), ylim=c(0,yMax), col="#ffffff")
par(new=T)
allData[,1] <- as.numeric(as.character(allData[,1]))
# plot(allData, pch=15, xlim=c(1,12), ylim=c(0,yMax), col="#00ff0060")
# par(new=T)

# par(family = "HiraKakuPro-W3", bty = "l")
# plot(dfList7[["酒田"]]$month, dfList7[["酒田"]]$降水量, xlim=c(1,12), ylim = allylim, lty=1)
# par(new=T)
#
plot(
  ydata,
  type = "o",
  ylim = c(0,yMax),
  col = rgb(0, 0.7, 0),
  pch = 3,
  xlab = "月",
  ylab = axtxt
  , lty = 3
  , xlim=c(1,12)
)

# arrows(
#   c(1:12),
#   allAvr - allSD,
#   c(1:12),
#   allAvr + allSD,
#   code = 3,
#   lwd = 1,
#   angle = 90,
#   length = 0.1,
#   ylim = allylim,
#   lty = 1,
#   col = rgb(0, 0.7, 0)
# )
