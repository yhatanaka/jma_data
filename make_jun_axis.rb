#!/usr/bin/ruby
require 'pp'
jun = [1,11,21]
month = [1,2,3,4,5,6,7,8,9,10,11,12]

result_ary = []
month.each do |this_month|
	jun.each do |this_jun|
		result_ary << this_month.to_s + '/' + this_jun.to_s  
	end
end
print result_ary