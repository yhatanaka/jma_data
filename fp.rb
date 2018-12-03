#!/usr/bin/ruby
require 'pp'

input_file = ARGV.shift
output_file = ARGV.shift


# code_ary : [chunk_hash_1, chunk_hash_2, ...]
# chunk-hash : hash: {
# 	hash_name => # >> 'hoge'
# 	common_sentence => array : [
# 		'hogehuga'
# 		, 'hogehuga'
# 		, 'mogamoge'
#	]
# 	func => array: [
# 		'def hoge(huga) {
# 			moga
# 		}'
# 		, 'def moge() {
# 			guga
# 		}'
#	]
#	proc => array: [
#		'hoge = huga
#		moge = moga'
#		, 'guge = guga + gego'
#	]

def make_a_chunk_ary(str, cs_ary, f_ary, p_ary)
	result = Hash.new
	result['hash_name'] = str
	result['common_sentence'] = cs_ary
	result['func'] = f_ary
	result['proc'] = p_ary
	return result
end #def
 
def seperate_all_chunk(strs)
# chunk 名: 
# >> hoge
	chunk_regex = /\#\s*>>\s*(.+)\s*/
# 関数部分: 
# >> func
	func_regex = /\#\s*>>\s*func/
# 処理部分: 
# >> proc
	proc_regex = /\#\s*>>\s*proc/
	 
	code_ary = Array.new
	chunk_name =''
# chunk 名以外をクリア
	common_sentence_ary = Array.new
	fp_mode = ''
	func_ary = Array.new
	proc_ary = Array.new
	chunk_hash = {}

	strs.each do |line|
# モード切り替え
		if line =~ func_regex
			fp_mode = 'func'
# モード切り替え
		elsif line =~ proc_regex
			fp_mode = 'proc'
# chunk_regex に該当すると，func_regex，proc_regex にもヒットしてしまうので，最後に
		elsif line =~ chunk_regex
# 2つ目以降の chunk なら，前の chunk を hash にして追加してから, chunk_name 更新
			if chunk_hash.size > 0
				chunk_hash = make_a_chunk_ary(chunk_name, common_sentence_ary, func_ary, proc_ary)
				code_ary.push(chunk_hash)
# chunk 名以外をクリア
				common_sentence_ary = Array.new
				fp_mode = ''
				func_ary = Array.new
				proc_ary = Array.new
				chunk_hash = {}
			end #if
			chunk_name = $1.strip
# モード切り替え
			fp_mode = 'common'
		else
# 切り替えたモードに応じて，ソースを適当な配列に追加
			case fp_mode
			when 'common' then
				common_sentence_ary.push(line)
			when 'func' then
				func_ary.push(line)
			when 'proc' then
				proc_ary.push(line)
			else
				
			end
		end
# 最後の chunk を追加
		chunk_hash = make_a_chunk_ary(chunk_name, common_sentence_ary, func_ary, proc_ary)
	end # f.each
	return code_ary.push(chunk_hash)
end #def


def seperate_func_proc(chunk_ary)
	result = Array.new
	chunk_ary.each do |chunk|
		result.push('# >> ' + chunk['hash_name'] + " >>func>>")
		result.push(chunk['common_sentence'].join("\n") )
		result.push(chunk['func'].join("\n"))
	end # each
	return result.join("\n")
end #def


if input_file
	open(input_file,'r') do |f|
		seperate_all_chunk(f)
	end # do input_file
end #if

