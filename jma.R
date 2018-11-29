# データ読み込み ファイル名指定
f <-
  read.table(
    'data.csv',
    sep = ",",
    skip = 1,
    header = F,
    fileEncoding = "Shift_JIS",
    stringsAsFactors = F
  )

# 今回は全部酒田のデータなので， 1行目の「酒田」はじゃま。
f <- f[-1, ]

# 「(℃)」「(mm)」「の平均」「の合計」削除
removeCharFromTitle1 <- function(string) {
  return(gsub("(\\(.+\\))|(の.+)", "", string))
}

removeCharFromTitle2 <- function(string) {
  return(gsub("(情報)|(番号)", "", string))
}

# 各列の1行目_2行目
makeColumnName <- function(dfCol) {
  retCol = removeCharFromTitle1(dfCol[1])
  if (dfCol[2] != "") {
    retCol = paste(retCol, removeCharFromTitle2(dfCol[2]), sep = "_")
  }
  return(retCol)
}
# タイトルに設定
colnames(f) <- apply(f, 2, makeColumnName)

# 1((平均気温だの),2(均質情報だの)行目削除
f <- f[-1:-2, ]

# 「年/月」を，その月の1日の日付型に。
f[, 1] <- as.Date(paste(f[, 1], "/1", sep = ""))


# "年/月" の月の部分，"month"の列にfactor型で
# まず月の部分取り出す
monthLabel <- function(x) {
  return(format(x, "%m"))
}

f$month <- as.factor(sapply(f$年月, monthLabel))

# 「_」でつないだとこ（品質情報，均質番号，現象なし）はfactor型に
# 「_」でつないだ列を返す関数作ってとりあえず泥臭く for で回す
combinedColumn <- function(df) {
  return(grepl(".+_.+", colnames(df)))
}

for (i in which(combinedColumn(f))) {
  f[, i] <- as.factor(f[, i])
}

# 「平均気温」「日最高気温」「降水量」などのデータ部分は numeric型に変換
# (colnames が「年月」「month」以外で，しかも「_」ない)の列
dataColumn <- function(df) {
  dataFlag1 = !grepl("(.+_.+)", colnames(df))
  dataFlag2 = !grepl("(年月)|(month)", colnames(df))
  return(as.logical(dataFlag1 * dataFlag2))
}

for (i in which(dataColumn(f))) {
  f[, i] <- as.numeric(f[, i])
}

# 項目名
colnames(f)[dataColumn(f)]

# ついでに年月の範囲
paste(format(min(f$年月), "%Y/%m"), format(max(f$年月), "%Y/%m"), sep = " 〜 ")

#par(family="HiraKakuPro-W3", bty="l", lty=1)
#hist(f$平均気温)

# avrTempByMonth1 <- aggregate(x=f[c("平均気温")], by=list(f$month), FUN=mean)
# avrTempByMonth <- avrTempByMonth1$平均気温
# names(avrTempByMonth) <- avrTempByMonth1$Group.1
# avrTempByMonth <- tapply(f$平均気温, f$month, mean)
# barplot(avrTempByMonth)
# varTempByMonth1 <- tapply(f$平均気温, f$month, sd)
# bp <- barplot(avrTempByMonth, ylim = c(0, max(avrTempByMonth + varTempByMonth1)))
#
# arrows(bp, avrTempByMonth - varTempByMonth1, bp, avrTempByMonth + varTempByMonth1, code = 3, lwd = 1, angle = 90, length = 0.1)

# f2018 <- subset(f, grepl("2018", format(f$年月, "%Y")))
# plot(f2018[,c("month","平均気温")], type="o", ylim=c(-10,35), col=rgb(0,0.7,0), pch=1, xlab="月", ylab="気温")
# arrows(c(1:12), avrTempByMonth - varTempByMonth1, c(1:12), avrTempByMonth + varTempByMonth1, code = 3, lwd = 1, angle = 90, length = 0.1, ylim=c(-10,35), lty=1, col=rgb(0,0.7,0))

# 最大値の次から12までの連続値
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
  avrByMonth <- tapply(jmaDF[, c(itemName)], jmaDF$month, mean)
  sdByMonth <- tapply(jmaDF[, c(itemName)], jmaDF$month, sd)
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
pldata <- plotDotLine(f, "2017", "降水量")
ydata <- pldata$data
allAvr <- pldata$avr
allSD <- pldata$sd
allylim <- pldata$ylim
axtxt <- pldata$axtxt

par(family = "HiraKakuPro-W3", lty = 3, bty = "l")
plot(
  ydata,
  type = "o",
  ylim = allylim,
  col = rgb(0, 0.7, 0),
  pch = 1,
  xlab = "月",
  ylab = axtxt
)
arrows(
  c(1:12),
  allAvr - allSD,
  c(1:12),
  allAvr + allSD,
  code = 3,
  lwd = 1,
  angle = 90,
  length = 0.1,
  ylim = allylim,
  lty = 1,
  col = rgb(0, 0.7, 0)
)


# list("地名" => list( "項目名" => 年月,項目名,均質だのなんだかんだ,month) )
