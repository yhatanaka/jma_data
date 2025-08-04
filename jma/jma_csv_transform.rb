#!/usr/bin/ruby
# -*- coding: utf-8 -*-
#$KCODE='UTF8'
Encoding.default_external = "UTF-8"
# Encoding.default_internal = "UTF-8"

# usage: ruby guide_overlap_check.rb input.csv
# 気象庁WebサイトからダウンロードしたCSVファイルを、R用に整形

require 'pp'

# No  ,名称,ｐH,EC（μS）,水温（℃）,種類,lon,lat
inputFile = '/Users/hatanaka/Documents/jma_data/2025-08-03_飛島の気候と植物/allData_月-分割.csv'
#inputFile = ARGV.shift
inputCsv = CSV.read(inputFile, headers: true)
#outputFile = '/Users/hatanaka/Dropbox/遊佐町/水資源条例/spring4gmt.tsv'


class Jma_csv
# 気象庁WebサイトからダウンロードしたCSVファイルを、R用に整形
	require 'csv'
	attr_accessor :inputCsv
	
	def initialize(inputFile)
# Shift_jis のcsvファイルから、項目名をシンボルにしたCSVTable
	
	end #initialize


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
# 例外は小さい単位で捕捉する
rescue SystemCallError => e
  puts %Q(class=[#{e.class}] message=[#{e.message}])
rescue IOError => e
  puts %Q(class=[#{e.class}] message=[#{e.message}])
end
=begin


=end