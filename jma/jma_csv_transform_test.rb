!/usr/bin/ruby
# -*- coding: utf-8 -*-
#$KCODE='UTF8'
Encoding.default_external = "UTF-8"
# Encoding.default_internal = "UTF-8"

require 'minitest/autorun' # Minitest ライブラリを読み込む
require_relative './jma_csv_transform' # テストしたいファイルを読み込む

class JmaCdvTransformTest < Minitest::Test # JmaCdvTransformTest クラスを定義、Minitest::Test クラスを継承
	def test_jma_csv_transform
	
	end #test_jma_csv_transform
end #class