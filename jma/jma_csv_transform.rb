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
	attr_accessor :inputCsv
	
	def initialize(inputFile)
# Shift_jis のcsvファイルから、項目名をシンボルにしたCSVTable
		header_converter = lambda {|h| h.to_sym}
		inputFileContents = IO.read(inputFile, encoding: 'SJIS').encode('UTF-8')
		@inputCsv = CSV.parse(inputFileContents, headers: false, skip_blanks: true)
#		@inputCsv = CSV.parse(inputFileContents, headers: false, skip_blanks: true, header_converters: header_converter)
	end
	def compositData(data_tbl)
		# >> 2. ヘッダ header 最終行
		headers_end = headerRowEnd(data_tbl)
		# >> 3. 年月日 ymd 最終列
		ymd_end = ymdColEnd(data_tbl)
		# >> 3. 開始日 最終列
		ymd_end2 = ymdColEnd2(data_tbl)
		# データのヘッダ部分削除
		
		databody_tbl = dataRows(data_tbl, headers_end)
		# 年月日の部分 + 開始年・月・日から日付の列 st_date
		# ymd_tbl = databody_tbl %>% ymdCols(ymd_end2) %>% mutate_all(as.factor)
		ymd_tbl = ymdCols(databody_tbl, ymd_end2)

	end
	def headerRowEnd
	
	end
	def ymdColEnd
	
	end
	def ymdColEnd2
	
	end
	def test
		return @inputCsv
	end
end #class


#getHeaders = ['lon', 'lat', 'EC（μS）']
#
#returnHeaders = ['経度', '緯度', '電気伝導度']
#returnHeadersRow = CSV::Row.new(returnHeaders, [], header_row: true)
#outputCSV = CSV::Table.new([returnHeadersRow])
#
#inputCsv.each {|row|
#	addRow = []
#	getHeaders.each {|aHeader|
#		addRow << row[aHeader]
#	}
#	outputCSV << addRow
#}
#
#
#IO.write(outputFile, outputCSV.to_csv(col_sep: ',', write_headers: true))
#print outputCSV.to_csv(col_sep: "\t")

begin

pp 'test'

aJma_csv = Jma_csv.new(inputFile)
puts aJma_csv.test.to_s
# 例外は小さい単位で捕捉する
rescue SystemCallError => e
  puts %Q(class=[#{e.class}] message=[#{e.message}])
rescue IOError => e
  puts %Q(class=[#{e.class}] message=[#{e.message}])
end
=begin


=end