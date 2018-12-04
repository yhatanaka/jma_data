#!/usr/bin/ruby
require 'pp'

input_file = ARGV.shift
output_file = ARGV.shift


# chunk_ary : [chunk_hash_1,chunk_hash_2,...]
# chunk-hash : hash: {
# 	hash_name => # >> 'hoge'
# 	common_sentence => array : [
# 		'hogehuga'
# 		,'hogehuga'
# 		,'mogamoge'
#	]
# 	decl => array: [
# 		'hoge = "hega"'
# 		,'fuga = "hege"'
#	]
# 	func => array: [
# 		'def hoge(huga) {
# 			moga
# 		}'
# 		,'def moge() {
# 			guga
# 		}'
#	]
#	proc => array: [
#		'hoge = huga
#		moge = moga'
#		,'guge = guga + gego'
#	]

class CodeSeperater
	$chunk_ary = Array.new

	def make_a_chunk_ary(str, cs_ary, d_ary, f_ary, p_ary)
		result = Hash.new
		result['hash_name'] = str
		result['common_sentence'] = cs_ary
		result['decl'] = d_ary
		result['func'] = f_ary
		result['proc'] = p_ary
		return result
	end #def
	 
	def fromChunkedToAry(f)
		# chunk 名: 
		# >> hoge
		chunk_regex = /\#\s*>>\s*(.+)\s*/
		# 宣言部分: 
		# >> decl
		decl_regex = /\#\s*>>\s*decl/
		# 関数部分: 
		# >> func
		func_regex = /\#\s*>>\s*func/
		# 処理部分: 
		# >> proc
		proc_regex = /\#\s*>>\s*proc/
		
		$chunk_ary = Array.new
		chunk_name =''
		# chunk 名以外をクリア
		common_sentence_ary = Array.new
		fp_mode = ''
		decl_ary = Array.new
		func_ary = Array.new
		proc_ary = Array.new
		chunk_hash = {}

		f.each do |line|
			# モード切り替え
			if line =~ decl_regex
				fp_mode = 'decl'
			# モード切り替え
			elsif line =~ func_regex
				fp_mode = 'func'
			# モード切り替え
			elsif line =~ proc_regex
				fp_mode = 'proc'
			# chunk_regex に該当すると，func_regex，proc_regex にもヒットしてしまうので，最後に
			elsif line =~ chunk_regex
				# 2つ目以降の chunk なら，前の chunk を hash にして追加してから, chunk_name 更新
				if chunk_hash.size > 0
					chunk_hash = make_a_chunk_ary(chunk_name, common_sentence_ary, decl_ary, func_ary, proc_ary)
					$chunk_ary.push(chunk_hash)
					# chunk 名以外をクリア
					common_sentence_ary = Array.new
					fp_mode = ''
					decl_ary = Array.new
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
				when 'decl' then
					decl_ary.push(line)
				when 'func' then
					func_ary.push(line)
				when 'proc' then
					proc_ary.push(line)
				else
					
				end
			end
			# 最後の chunk を追加
			chunk_hash = make_a_chunk_ary(chunk_name, common_sentence_ary, decl_ary, func_ary, proc_ary)
		end # f.each
		$chunk_ary.push(chunk_hash)
	end

	def fromChunked(f)
		fromChunkedToAry(f)
		return $chunk_ary
	end

	def toSeperated(f)
		fromChunkedToAry(f)
		toSeperatedFromAry()
	end

	def makePartCodeFromAry(srcHash, str, rsltAry)
		if srcHash[str].size > 0
			rsltAry.push('# >> ' + srcHash['hash_name'] + ' >>' + str + '>>')
			if srcHash['common_sentence'].size > 0
				rsltAry.push(srcHash['common_sentence'])
			end #if
			rsltAry.push('# >>' + str)
			rsltAry.push(srcHash[str])
		end #if
	end #def

	def toSeperatedFromAry()
		decl_ary = Array.new
		func_ary = Array.new
		proc_ary = Array.new
		$chunk_ary.each do |chunk|
			makePartCodeFromAry(chunk, 'decl', decl_ary)
			makePartCodeFromAry(chunk, 'func', func_ary)
			makePartCodeFromAry(chunk, 'proc', proc_ary)
		end # each
		return print_arys([decl_ary, ["\n# <<< decl : func >>>\n"], func_ary, ["\n# <<< func : proc >>>\n"], proc_ary])

	end

	def print_arys(arys)
		return arys.flatten.join("\n")+ "\n"
	end #def

	def fromSeperated(f)
		fromSeperatedToAry(f)
		return $chunk_ary
	end

	def fromSeperatedToAry(f)
		# chunk 名: 
		# >> hoge
		chunk_regex = /\#\s*>>\s*(.+)\s*/
		# 関数部分: 
		# >> func
		func_regex = /\#\s*>>\s*func/
		# 処理部分: 
		# >> proc
		proc_regex = /\#\s*>>\s*proc/
		
		$chunk_ary = Array.new
		chunk_name =''
		# chunk 名以外をクリア
		common_sentence_ary = Array.new
		fp_mode = ''
		func_ary = Array.new
		proc_ary = Array.new
		chunk_hash = {}

		f.each do |line|
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
					$chunk_ary.push(chunk_hash)
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
		$chunk_ary.push(chunk_hash)
	end
end #class

if input_file
	open(input_file,'r') do |f|
		seperate_all_chunk(f)
	end # do input_file
end #if

__END__
