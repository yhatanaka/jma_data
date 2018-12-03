#!/usr/bin/ruby
require 'minitest/autorun'

require './fp'

$test1 =<<EOS.split("\n")
# >> 1. データ読み込み
EOS
$result1 = [{'hash_name' => '1. データ読み込み', 'common_sentence' => [], 'func' => [], 'proc' => []}]

$test2 =<<EOS.split("\n")
# >> 1. データ読み込み
# テスト
  test # テスト2
EOS
$result2 = [{'hash_name' => '1. データ読み込み', 'common_sentence' => ['# テスト', '  test # テスト2'], 'func' => [], 'proc' => []}]

$test3 =<<EOS.split("\n")
# >> 1. データ読み込み
# テスト
  test # テスト2
# >>func
read_jma <- function(csv_f) {
      data_file
  # 空行削除
EOS
$result3 = [{'hash_name' => '1. データ読み込み', 'common_sentence' => ['# テスト', '  test # テスト2'], 'func' => ['read_jma <- function(csv_f) {', '      data_file', '  # 空行削除'], 'proc' => []}]

$test4 =<<EOS.split("\n")
# >> 1. データ読み込み
# テスト
  test # テスト2
# >>func
read_jma <- function(csv_f) {
      data_file
  # 空行削除
# >>proc
data_tbl <- read_jma(data_file)
EOS
$result4 = [{'hash_name' => '1. データ読み込み', 'common_sentence' => ['# テスト', '  test # テスト2'], 'func' => ['read_jma <- function(csv_f) {', '      data_file', '  # 空行削除'], 'proc' => ['data_tbl <- read_jma(data_file)']}]

$test5 =<<EOS.split("\n")
# >> 1. データ読み込み
# テスト
  test # テスト2
# >>func
read_jma <- function(csv_f) {
      data_file
  # 空行削除
# >>proc
data_tbl <- read_jma(data_file)

# >> 2. ヘッダ header 

EOS
$result5 = [{
        'hash_name' => '1. データ読み込み', 'common_sentence' => ['# テスト', '  test # テスト2'], 'func' => ['read_jma <- function(csv_f) {', '      data_file', '  # 空行削除'], 'proc' => ['data_tbl <- read_jma(data_file)', '']
},{'hash_name' => '2. ヘッダ header', 'common_sentence' => [], 'func' => [], 'proc' => []}]


$test6 =<<EOS.split("\n")
# >> 1. データ読み込み
# テスト
  test # テスト2
# >>func
read_jma <- function(csv_f) {
      data_file
  # 空行削除
# >>proc
data_tbl <- read_jma(data_file)

# >> 2. ヘッダ header 
# >>func
        func_2
        test
# >>proc
        proc_2
                testetst
EOS
$result6 = [{
        'hash_name' => '1. データ読み込み', 'common_sentence' => ['# テスト', '  test # テスト2'], 'func' => ['read_jma <- function(csv_f) {', '      data_file', '  # 空行削除'], 'proc' => ['data_tbl <- read_jma(data_file)', '']
},{'hash_name' => '2. ヘッダ header', 'common_sentence' => [], 'func' => ['        func_2', '        test'], 'proc' => ['        proc_2', '                testetst']}]

# ---------------------

$test7 =<<EOS.split("\n")
# >> 1. データ読み込み
# テスト
  test # テスト2
# >>func
read_jma <- function(csv_f) {
      data_file
  # 空行削除
# >>proc
data_tbl <- read_jma(data_file)

# >> 2. ヘッダ header 
# >>func
        func_2
        test
# >>proc
        proc_2
                testetst
EOS

$result7 =<<EOS
# >> 1. データ読み込み >>func>>
# テスト
  test # テスト2
read_jma <- function(csv_f) {
      data_file
  # 空行削除
# >> 2. ヘッダ header >>func>>
        func_2
        test
# >> 1. データ読み込み >>proc>>
# テスト
  test # テスト2
data_tbl <- read_jma(data_file)

# >> 2. ヘッダ header >>proc>>
        proc_2
                testetst
EOS


class FooTest < Minitest::Test
  def test_1
    assert_equal $result1, seperate_all_chunk($test1)
  end
  def test_2
    assert_equal $result2, seperate_all_chunk($test2)
  end
  def test_3
    assert_equal $result3, seperate_all_chunk($test3)
  end
  def test_4
    assert_equal $result4, seperate_all_chunk($test4)
  end
  def test_5
    assert_equal $result5, seperate_all_chunk($test5)
  end
  def test_6
    assert_equal $result6, seperate_all_chunk($test6)
  end
end

class SeperateTest <Minitest::Test
        def test_sep7
                assert_equal $result7, seperate_func_proc(seperate_all_chunk($test7))
        end
end