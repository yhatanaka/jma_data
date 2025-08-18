#!/usr/bin/ruby
# -*- coding: utf-8 -*-
#$KCODE='UTF8'
Encoding.default_external = "UTF-8"
# Encoding.default_internal = "UTF-8"

# usage: ruby guide_overlap_check.rb input.csv
# 気象庁WebサイトからダウンロードしたCSVファイルを、R用に整形

require 'pp'

# No  ,名称,ｐH,EC（μS）,水温（℃）,種類,lon,lat
inputFile = '/Users/hatanaka/Documents/jma_data/2025-08-03_飛島の気候と植物/csv_unused/data-51.csv'
# '/Users/hatanaka/Documents/jma_data/2025-08-03_飛島の気候と植物/allData_月-分割.csv'
#inputFile = ARGV.shift
#inputCsv = CSV.read(inputFile, headers: true)
#outputFile = '/Users/hatanaka/Dropbox/遊佐町/水資源条例/spring4gmt.tsv'


class Jma_csv
# 気象庁WebサイトからダウンロードしたCSVファイルを、R用に整形
	require 'csv'
	attr_accessor :inputCsv, :headersYmdAry, :dataYmdAry, :headersObsrvRaw, :headers_end, :dataObsrvAry, :ymdTbl, :placeUniqAry, :headersObsrvAry, :unpivotedExtraHeaders
# テスト用
	@headersYmdAry
	@dataYmdAry

	def initialize(inputFile)
# Shift_jis のcsvファイルから、項目名をシンボルにしたCSVTable
#		header_converter = lambda {|h| h.to_sym}
		inputFileContents = IO.read(inputFile, encoding: 'SJIS').encode('UTF-8')
		@inputCsv = CSV.parse(inputFileContents, headers: false, skip_blanks: true)
		if @inputCsv[0][0] =~ /^ダウンロードした時刻：/
			@inputCsv.delete_at(0)
		end
# 縦持ちにした時の、key 項目以外のヘッダ名
		@unpivotedExtraHeaders = [:key, :value]
#		@inputCsv = CSV.parse(inputFileContents, headers: false, skip_blanks: true, header_converters: header_converter)
	end

# R用データ
	def compositData()
# >> 2. ヘッダ header 最終行
		headers_end = headerRowEnd()
# >> 3. 年月日 ymd 最終列
		ymd_end = ymdColEnd()
# >> 3. 開始日 最終列
#		ymd_end2 = ymdColEnd2(data_tbl)

#         | ymd | Obsrv |
# headers |     |        |
# data    |     |        |
# データ行 dataRows とヘッダ行 headerRows 分離: :headers, :data
		headersAndDataAry = separateRows(headers_end)
# ヘッダ行 年月日部分 :ymd 、観測データ :data
		headersYmdRaw = separateColumns(headersAndDataAry[:headers], ymd_end)
# ymd
		ymdHash = {'年' => 'y', '月' => 'm', '日' => 'd', '時' => 'h'}
# 「年」「月」などは2行目([1])、y,m,d etc.に変換、start_ つける makeYmdClmName
		@headersYmdAry = headersYmdRaw[:ymd][1].map {|n|
			'start_' + ymdHash[n]
		}
# data 観測値
		headersObsrvRaw = headersYmdRaw[:data]
# 1行目は観測地点名、2行目以降が項目名
		placeAry = headersObsrvRaw.shift
		@headersObsrvAry = makeObsrvClmName(headersObsrvRaw)

# データ行 年月日部分 :ymd 、観測データ :data
		dataYmdRaw = separateColumns(headersAndDataAry[:data], ymd_end)
		@dataYmdAry = dataYmdRaw[:ymd]
		dataObsrvAry = dataYmdRaw[:data]

#		年月日の部分 + 開始年・月・日から日付の列 st_date
		ymdTbl_raw = makeTable(@headersYmdAry, @dataYmdAry)
		ymdTbl = addSt_date2Ymd(ymdTbl_raw)

# CSV.Table に
#		@placeUniqAry = placeAry.uniq
# 観測地点名ごとにデータを分割
		placeHeadersDataHashAry = divideDataByPlace(dataObsrvAry, @headersObsrvAry, placeAry)
# 観測地点ごとのヘッダと観測値から、観測地点ごとの Table 作る
		result = makePlaceTblHash(placeHeadersDataHashAry[0], placeHeadersDataHashAry[1])
		resultAry = []
		keyHeaders = [:start_y, :start_m, :st_date, :place]
		result.each {|placeDataTbl|
# 年月日に観測データ付け加え、観測地点ごとに格納
			ymdAndDataTbl = ymdTbl.by_col
			placeDataTbl.by_col.each {|col|
				ymdAndDataTbl[col[0]] = col[1]
			}
			ymdAndDataTbl.by_row!
			resultAry << unpivot(ymdAndDataTbl, keyHeaders)
		}

		unpivottedAllData = []
		jointedTbl = CSV::Table.new([])
#		resultAry.each_with_index {|table|
#			unpivottedAllData << unpivot(table, keyHeaders)
#		}

		resultAry.each_with_index {|n, idx|
			if idx == 0
				jointedTbl = n.by_row
			else
				n.by_row.each {|row|
					jointedTbl << row
				}
			end
		}
#		return result
		return pivot(jointedTbl, keyHeaders)
	end

	def headerRowEnd()
# ヘッダとして扱うのは何行目までか
# 先頭から"NA"が続く最後の行までが項目名。その後のちゃんと数字が始まる（NA じゃない）行からがデータ。
# ヘッダの行の最後は，先頭列が数字である一番始めの（つまりデータの始まる）行の一つ前まで
		@inputCsv.each_with_index {|rawRow, idx|
			if rawRow[0] =~ /^[0-9]{4}$/
				return idx -1
				break
			end
		}
	end

	def ymdColEnd()
# 年月日の列の最後は，地名が始まる一番最初の列(notYmdCol が TRUE になる最初の列)の，1つ前
		@inputCsv[0].each_with_index {|placeRowColumn, idx|
			if !placeRowColumn.nil?
				return idx -1
				break
			end
		}
	end

# データ行 dataRows とヘッダ行 headerRows 分離 : :headers, :data
	def separateRows(headersRowEndNum)
		headerRows = @inputCsv.shift(headersRowEndNum+1)
		dataRows = @inputCsv
		return {:headers => headerRows, :data => dataRows}
	end

# 年月日部分 :ymd 、観測データ :data
	def separateColumns(aryOfAry, ymdColEndNum)
		ymdCols = []
		dataCols = []
		aryOfAry.each {|rawRow|
			ymdCols << rawRow.shift(ymdColEndNum+1)
			dataCols << rawRow
		}
		return {:ymd => ymdCols, :data => dataCols}
	end

# 配列の配列と、ヘッダの配列から、CSV::table へ変換
	def makeTable(headersAry, bodyAryOfAry)
		bodyRowAry = []
		bodyAryOfAry.each {|bodyRow|
			bodyRowAry << CSV::Row.new(headersAry.map {|name| name.to_sym}, bodyRow)
		}
		return CSV::Table.new(bodyRowAry)
	end

# 年月日の列から st_data 列を作り、追加
	def addSt_date2Ymd(ymdTbl)
		st_dateAry = ymdTbl.map {|row|
			row.to_h.values.join('/')
		}
		ymdTbl.by_col!
		ymdTbl[:st_date] = st_dateAry
		return ymdTbl.by_row
	end

# >>  5. 気象のヘッダから列名
# 2行目[1]の「(時間)」「(℃)」「(mm)」「の平均」「の合計」削除
	def rmvStr_1(str)
		if str.nil?
			return nil
		else
			return str.sub(/の.+/, '').sub(/\(.+/, '')
		end
	end
# 3行目[2]の「番号」「情報」削除
	def rmvStr_2(str)
		if str.nil?
			return nil
		else
			return str.sub('情報', '').sub('番号', '')
		end
	end
# 観測値ヘッダ。転置、rmvStr_1, rmvStr_2 で削除した2行目〜くっつけて列名に
	def makeObsrvClmName(aryOfAry)
		transposed = aryOfAry.transpose
		modAryOfAry = transposed.map {|obsrvCols|
			substrAry = obsrvCols.map {|rawColRow|
				rmvStr_2(rmvStr_1(rawColRow))
			}
# nil は削除し、'_'で連結
			substrAry.compact.join('_')
		}
		return modAryOfAry
	end

# 観測地点名ごとにデータを分割
# 各観測地点名名をキー、1行目項目名で2行目以降観測値の2次元配列を Hash に
# {地点名1 => [[項目名1, 項目名2, ...],
#              [観測値11, 観測値12, ...],
#              [観測値21, 観測値22, ...],
#                        ：
#             ],
#  地点名2 => [[項目名1, 項目名2, ...],
#              [観測値11, 観測値12, ...],
#              [観測値21, 観測値22, ...],
#                        ：
#             ],
#                        ：
# }
	def divideDataByPlace(bodyAryOfAry, headersAry, placeAry)
# 観測地点名集約
		placeAryUniq = placeAry.uniq
		placeDividedDataHash = {}
		placeHeadersHash = {}
		placeIndexHash = {}
# 観測地点ごとの、データ配列 placeDividedDataHash と、ヘッダ配列 placeHeadersHash と、該当箇所を示す配列 placeIndexHash を準備
		placeAryUniq.each {|aPlace|
			placeDividedDataHash[aPlace] = []
			placeHeadersHash[aPlace] = []
			placeIndexHash[aPlace] = []
		}
# 観測地点名のヘッダ行から、各観測地点のデータが[どの列から, いくつあるか]の配列 placeIndexHash を作る
# array から、arry[placePosAry[0], placePosAry[1]]で各観測地点名に対応した部分配列を取り出せる
		placeAry.each_with_index {|aPlace, idx|
			if placeIndexHash[aPlace].count == 0
				placeIndexHash[aPlace] = [idx, 1]
			else
				placeIndexHash[aPlace][1] += 1
			end
		}

# まず、ヘッダとして各観測地点ごとの観測項目名
		placeIndexHash.each {|aPlace, placePosAry|
			placeHeadersHash[aPlace] << headersAry[placePosAry[0], placePosAry[1]]
		}
#pp placeHeadersHash
# データの各行 aRow から各観測地点名の部分を抽出 aRow[placePosAry[0], placePosAry[1]] した配列を、各行ごとの配列にして観測地点ごとの Hash (placeDividedDataHash[aPlace]) に格納
		bodyAryOfAry.each {|aRow|
			placeIndexHash.each {|aPlace, placePosAry|
				placeDividedDataHash[aPlace] << aRow[placePosAry[0], placePosAry[1]]
			}
		}
# 確認用 観測地点名の配列を分割し、正しく地点ごとに分割できるか確認
#		placeIndexHash.each {|aPlace, placePosAry|
#			placeDividedDataHash[aPlace] << placeAry[placePosAry[0], placePosAry[1]]
#		}
#pp placeDividedDataHash
		return [placeHeadersHash, placeDividedDataHash]
	end

# divideDataByPlace の結果から、観測地点ごとに table
	def makePlaceTblHash(headerHash, bodyHash)
		resultAry = []
		headerHash.each {|place, headersAry|
			thisPlaceData = bodyHash[place]
# [[~~, ~~, ~~, …]]
			table = makeTable(headersAry[0], bodyHash[place]).by_col
			placeColAry = Array.new(thisPlaceData.count, place)
			table[:place] = placeColAry
			resultAry << table
		}
		return resultAry
	end

# 縦持ちにした時の、key 項目以外のヘッダ名
#	@unpivotedExtraHeaders = [:key, :value]
#	unpivoted = unpivot(table, [:名前, :年月日])
#	pp unpivoted.headers
#	pivoted = pivot(unpivoted, [:名前, :年月日])

# 縦持ちに
	def unpivot(table, keysAry)
# 縦持ちのヘッダ、(keysAry の各項目), unpivotedExtraHeaders
		newHeadersAry = keysAry + @unpivotedExtraHeaders
# 最終的なデータ
		outputRowsAry = []
	
		table.by_row.each {|row|
# この行の key 項目のデータを入れる
			keyCols = []
			rowHash = row.to_h
# key 項目それぞれで…
			keysAry.each {|keyHeader|
# key 項目のデータを格納し…
				keyCols << rowHash[keyHeader]
# 格納したら行のデータからは削除
				rowHash.delete(keyHeader)
			}
# 残ったもの、つまり行の key 項目「以外」のヘッダ名と値を…
			rowHash.each {|key, value|
# 値が nil でなければ
				if value
					outputRow = []
# ヘッダ名と値を…
					outputRow.push(key,value)
# key 項目に続けて、1行できあがり
					outputRowsAry << keyCols + outputRow
				end
			}
		}
#	pp keysAry.push(:key)
# 行のデータと、ヘッダから table 作る
		return makeTable(newHeadersAry, outputRowsAry)
	end
	
	
# 横持ちに
	def pivot(table, keysAry)
		rowsAry = []
		keysHash = {}
# 横持ちの場合の項目名
		table.by_col!
		extraPivotedHeadersName = table[:key].uniq
	
# 行ごとに…
		table.by_row!
		table.each {|row|
			rowHash = row.to_h
# この行の key 項目を入れる準備
			thisRowKeysAry = []
# key 項目ごとに…
			keysAry.each {|key|
# この行から拾って入れる
				thisRowKeysAry << rowHash[key]
# 入れたら元の行データからは削除
				rowHash.delete(key)
			}
# この key 項目は初めて出たんなら、追加のデータ入れる準備
			unless keysHash[thisRowKeysAry]
				keysHash[thisRowKeysAry] = {}
			end
	
# 残りのヘッダは、unpivotedExtraHeaders
# {["おれ", "今日"] => {昼食: "ラーメン", 夕食: "牛丼"},
# ["おれ", "昨日"] => {朝食: "梅干しご飯", おやつ: "ポテチ", 夕食: "ラーメン"},
# ["おれ", "一昨日"] => {朝食: "食パン", 昼食: "焼き魚定食", 夕食: "麻婆豆腐定食"},
# …}
			keysHash[thisRowKeysAry][rowHash[:key]] = rowHash[:value]
		}

# 最終的なヘッダは、key 項目 + (:key 列のuniq)
		pivotedHeadersName = keysAry + extraPivotedHeadersName

		resultAry = keysHash.each_with_object([]) {|(prmKeys, extraHash), newAry|
			rowAry = prmKeys
			extraPivotedHeadersName.each {|extraKey|
				rowAry << extraHash[extraKey]
			}
			newAry << rowAry
		}
	
		newHeaders = keysAry + extraPivotedHeadersName
		return makeTable(newHeaders, resultAry)
	end



# テスト用
	def dataYmdAry()
		return @dataYmdAry.to_s
	end

	def headersYmdAry()
		return @headersYmdAry.to_s
	end

	def headersObsrvAry()
		return @headersObsrvAry.to_s
	end


end #class


#returnHeadersRow = CSV::Row.new(returnHeaders, [], header_row: true)
#outputCSV = CSV::Table.new([returnHeadersRow])
#IO.write(outputFile, outputCSV.to_csv(col_sep: ',', write_headers: true))
#print outputCSV.to_csv(col_sep: "\t")

begin


	aJma_csv = Jma_csv.new(inputFile)
#		p aJma_csv.inputCsv.to_s
puts aJma_csv.compositData().to_csv
#pp aJma_csv.ymdTbl


# 例外は小さい単位で捕捉する
rescue SystemCallError => e
  puts %Q(class=[#{e.class}] message=[#{e.message}])
rescue IOError => e
  puts %Q(class=[#{e.class}] message=[#{e.message}])
end
=begin


=end