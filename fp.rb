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
		return print_arys([decl_ary, ['# <<< decl : func >>>'], func_ary, ['# <<< func : proc >>>'], proc_ary])

	end

	def print_arys(arys)
		return arys.flatten.join("\n")+ "\n"
	end #def

	def fromSeperated(f)
		pp fromSeperatedToAry(f)
		# return $chunk_ary
	end

	def fromSeperatedToAry(f)
		# chunk name
		chunk_regex = /\s*#\s*>>\s*(.+)\s*>>\s*(.+)\s*>>.*$/
		# 宣言・関数・処理部分: 
		# >> decl, func, proc
		part_regex = /\s*#\s*>>\s*(\S+)\s*/
		# パート decr, func, proc
		part_name_type_ary = ['decl', 'func', 'proc']
		# パート切り替え
		border_regex = /\s*#\s*<<<\s*(\S+)\s*:\s*(\S+)\s*>>>\s*/
		
		chunk_ary_temp = Array.new
		chunk_hash = {}
		chunk_name =''
		mode_temp = ''
		mode = ''
		mode_next = ''
		mode_check = ''
		# common_sentence_ary = []
		# part_hash = {'decr' => [], 'func' => [], 'proc' => []}
# result = ''
		f.each do |line|
			if line =~ chunk_regex
				chunk_name = $1
				unless chunk_hash.has_key?(chunk_name)
					chunk_hash[chunk_name] = {'hash_name' => chunk_name, 'cs' => [], 'decl' => [], 'func' => [], 'proc' => []}
				end #if
				mode_temp = $2
				mode = 'cs'
				# result += 'chunk: ' + chunk_name + ' mode: ' + $2 + " :\t" + line + "\n"
				next
			elsif line =~ border_regex
				# mode_check = $1
				# mode_next = $2
				# result += 'border: ' + $1 + ' : ' + $2 + " :\t" + line + "\n"
				next
			elsif line =~ part_regex
				if part_name_type_ary.include?($1)
					mode = $1
					# result += 'part: mode: ' + $1 + " :\t" + line + "\n"
				end #if
				next
			else
				if mode == 'cs'
					if chunk_hash[chunk_name]['cs'].size == 0
						chunk_hash[chunk_name]['cs'].push(line)
						# result += 'mode change: c.n: ' + chunk_name + ' part: cs' + " :\t" + line + "\n"
					end #if
					next
				elsif mode == mode_temp
					chunk_hash[chunk_name][mode].push(line)
					# result += 'mode: ' + mode + " :\t" + line + "\n"
				end #if
			end #if
		end # f.each
		# return result
		return chunk_hash
		# chunk_hash.each do |key, value|
		# 	chunk_ary_temp.push(value)
		# end #each
		# $chunk_ary = chunk_ary_temp
	end
end #class

if input_file
	open(input_file,'r') do |f|
		seperate_all_chunk(f)
	end # do input_file
end #if

__END__
