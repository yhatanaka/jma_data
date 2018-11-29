# データ読み込み ファイル名指定
f <-
  read.table(
    'data.csv',
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


# str(f)
placeWithNull <- unique(as.vector(as.matrix(f[1,])[1,]))
placeList <- placeWithNull[!is.na(placeWithNull)]
# placeList

dfList <- list()
changeToDate <- function(df){
  # 「年/月」を，その月の1日の日付型に。
  df <- as.Date(paste(as.character(df), "/1", sep = ""))
  return(as.data.frame(df))
}
# 1行目_2行目
removeCharFromTitle1 <- function(string) {
  return(gsub("(\\(.+\\))|(の.+)", "", string))
}
attachTitle <- function(string) {
  replacedTitleVec <- c("qlty", "unfm", "exst")
  names(replacedTitleVec) <- c("品質情報", "均質番号", "現象なし情報")
  return(replacedTitleVec[string])
}
makeColumnName <- function(dfCol) {
  if (is.na(dfCol[3])) {
    retCol = "data"
  } else {
    retCol = attachTitle(dfCol[3])
  }
  return(retCol)
}
# タイトルに設定
setCombinedColumn <- function(df){
  colnames(df) <- apply(df, 2, makeColumnName)
  return(df)
}

# year,month 付け加える
ymLabel <- function(x, formatString) {
  return(format(x, formatString))
}

for(i in placeList){
  # 先頭列は日付
  yearMonthDf <- f[,1]
  yearMonthDf <- changeToDate(yearMonthDf[c(-1:-3)])
  colnames(yearMonthDf) <- "date"
  yearMonthDf$year <- as.factor(ymLabel(yearMonthDf$date, "%Y"))
  yearMonthDf$month <- as.factor(ymLabel(yearMonthDf$date, "%m"))
  placeDf <- f[,which(f[1,] == i)]
  itemWithNull <- unique(as.vector(as.matrix(f[2,])[1,]))
  placeDfList <- list()
  for(j in itemWithNull){
    thisItemDf <- placeDf[,which(placeDf[2,] == j)]
    colnames(thisItemDf) <- apply(thisItemDf, 2, makeColumnName)
    thisItemDf <- thisItemDf[c(-1:-3),]
    placeDfList[[removeCharFromTitle1(j)]] <- cbind(yearMonthDf,thisItemDf)
  }
  dfList[[i]] <- placeDfList
}

typeChange <- function(x){
  # data はnumeric
  replaceColumnToNumeric <- c("data")
  # 以下は factor
  replaceColumnToFactor <- c("qlty", "unfm", "exst")
  columnNames <- names(x)
  # 列名の中で，これに含まれる列はこれこれにする
  for (i in replaceColumnToNumeric) {
    if (any(columnNames == i)) {
      x[,i] <- as.numeric(x[,i])
    }
  }
  for (i in replaceColumnToFactor) {
    if (any(columnNames == i)) {
      x[,i] <- as.factor(x[,i])
    }
  }
  return(x)
}
hierarchyFunc <- function(x){
  y <- lapply(x,typeChange)
  return(y)
}

dfList <- lapply(dfList, hierarchyFunc)


# プロット
dfPlot1 <- dfList[["酒田"]][["平均気温"]]
# dfPlot1_1 <- dfPlot1[dfPlot1$year == 2017,]
# 今年のデータ。
dfPlot1_2 <- dfPlot1[dfPlot1$year == 2018,]

# 列 data がnumだと箱ひげ
# plot(dfPlot1$month, dfPlot1$data)

# 列 data がcharacterだと散布図
# 全データ散布図
# plot(dfPlot1$month, as.character(dfPlot1$data), pch=15, col="#00ff0090")
# 特定の年のデータ，折れ線で
# plot(dfPlot2$month, as.character(dfPlot2$data), type="o", pch=1, col="#ff000090")

dfPlot2 <- dfList[["酒田"]][["日最高気温"]]

# dfPlot2_2 <- dfPlot2[dfPlot1$year == 2018,]

dfPlot3 <- dfList[["飛島"]][["日最高気温"]]
# dfPlot3_2 <- dfPlot3[dfPlot1$year == 2018,]


pchTypes <- c(3,1)
lineCols <- c("red", "blue")
lineTypes <- 3

xlimVec <- c(1,12)
yMax <- max(max(dfPlot2$data, na.rm = T),max(dfPlot3$data, na.rm = T))
yMin <- min(min(dfPlot2$data, na.rm = T),min(dfPlot3$data, na.rm = T),0)

quartz("test %d",10,10)

# ラベルはヒラギノ角ゴシW3，枠はL字形
par(family = "HiraKakuPro-W3", bty = "l")
# plot(dfPlot2$month, dfPlot2$data, xlim=xlimVec, ylim=c(yMin,yMax), xlab="月", ylab="最高気温", main="")
boxplot(dfPlot2$data ~ dfPlot2$month, xlim=xlimVec, ylim=c(yMin,yMax), notch=T, boxwex=0.4, at=(1:12)+0.2, border="#0000ff66", col="#0000ff30", xlab="月", ylab="最低気温", main="最低気温")
# points(dfPlot1$month, as.character(dfPlot1$data), pch=15, col="#00ff0060")
# points(dfPlot1_1$month, as.character(dfPlot1_1$data), type="b", pch=pchTypes[1], col=lineCols[1], lty=lineTypes)
par(new=T)
#  axes=F, ann=F で軸とラベル消す
# plot(dfPlot3$month, dfPlot3$data, xlim=xlimVec, ylim=c(yMin,yMax), border="#0000ff66", col="#0000ff30", axes=F, ann=F)
boxplot(dfPlot3$data ~ dfPlot3$month, xlim=xlimVec, ylim=c(yMin,yMax), notch=T, boxwex=0.4, at=(1:12)-0.2, border="#ff000066", col="#ff000030", axes=F, ann=F)

# points(dfPlot2_2$month, as.character(dfPlot2_2$data), type="b", pch=pchTypes[1], col=lineCols[1], lty=lineTypes)

par(xpd=T)
legend("topleft",c("酒田2018","飛島2018"), pch=pchTypes, col=lineCols, lty=lineTypes, bty="n")
