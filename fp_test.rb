#!/usr/bin/ruby
require 'minitest/autorun'

require __dir__ + '/fp'

$test1 =<<EOS.split("\n")
# >> 1. データ読み込み
EOS
$result1 = [{'hash_name' => '1. データ読み込み', 'common_sentence' => [], 'decl' => [], 'func' => [], 'proc' => []}]

$test2 =<<EOS.split("\n")
# >> 1. データ読み込み
# テスト
  test # テスト2
EOS
$result2 = [{'hash_name' => '1. データ読み込み', 'common_sentence' => ['# テスト', '  test # テスト2'], 'decl' => [], 'func' => [], 'proc' => []}]

$test3 =<<EOS.split("\n")
# >> 1. データ読み込み
# テスト
  test # テスト2
# >>func
read_jma <- function(csv_f) {
      data_file
  # 空行削除
EOS
$result3 = [{'hash_name' => '1. データ読み込み', 'common_sentence' => ['# テスト', '  test # テスト2'], 'decl' => [], 'func' => ['read_jma <- function(csv_f) {', '      data_file', '  # 空行削除'], 'proc' => []}]

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
$result4 = [{'hash_name' => '1. データ読み込み', 'common_sentence' => ['# テスト', '  test # テスト2'], 'decl' => [], 'func' => ['read_jma <- function(csv_f) {', '      data_file', '  # 空行削除'], 'proc' => ['data_tbl <- read_jma(data_file)']}]

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
$result5 = [ \
  { \
    'hash_name' => '1. データ読み込み' \
    , 'common_sentence' => ['# テスト', '  test # テスト2'] \
    , 'decl' => [] \
    , 'func' => ['read_jma <- function(csv_f) {', '      data_file', '  # 空行削除'] \
    , 'proc' => ['data_tbl <- read_jma(data_file)', ''] \
  },{ \
    'hash_name' => '2. ヘッダ header' \
    , 'common_sentence' => [] \
    , 'decl' => [] \
    , 'func' => [] \
    , 'proc' => [] \
  }
]


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
$result6 = [
  { \
    'hash_name' => '1. データ読み込み' \
    , 'common_sentence' => [ \
        '# テスト' \
        , '  test # テスト2' \
      ] \
    , 'decl' => [] \
    , 'func' => [ \
      'read_jma <- function(csv_f) {' \
      , '      data_file' \
      , '  # 空行削除' \
    ] \
    , 'proc' => [ \
      'data_tbl <- read_jma(data_file)' \
      , '' \
    ] \
  },{ \
    'hash_name' => '2. ヘッダ header' \
    , 'common_sentence' => [] \
    , 'decl' => [] \
    , 'func' => [ \
      '        func_2' \
      , '        test'
    ] \
    , 'proc' => [
      '        proc_2' \
      , '                testetst'
    ] \
  } \
]

# ---------------------

$result7 =<<EOS
# >> 1. データ読み込み >>func>>
# テスト
  test # テスト2
# >>func
read_jma <- function(csv_f) {
      data_file
  # 空行削除
# >> 2. ヘッダ header >>func>>
# >>func
        func_2
        test

# <<< func : proc >>>

# >> 1. データ読み込み >>proc>>
# テスト
  test # テスト2
# >>proc
data_tbl <- read_jma(data_file)

# >> 2. ヘッダ header >>proc>>
# >>proc
        proc_2
                testetst
EOS

$test8 =<<EOS.split("\n")
# >> 1. データ読み込み
# テスト
  test # テスト2
# >>func
read_jma <- function(csv_f) {
      data_file
  # 空行削除
# >>decl
  hoge = 'huga'
  moga = 'moge'
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
$result8_a = [{
        'hash_name' => '1. データ読み込み', 'common_sentence' => ['# テスト', '  test # テスト2'], 'decl' => ["  hoge = 'huga'", "  moga = 'moge'"], 'func' => ['read_jma <- function(csv_f) {', '      data_file', '  # 空行削除'], 'proc' => ['data_tbl <- read_jma(data_file)', '']
},{'hash_name' => '2. ヘッダ header', 'common_sentence' => [], 'decl' => [], 'func' => ['        func_2', '        test'], 'proc' => ['        proc_2', '                testetst']}]
$result8_b =<<EOS
# >> 1. データ読み込み >>decl>>
# テスト
  test # テスト2
# >>decl
hoge = 'huga'
moga = 'moge'
# >> 1. データ読み込み >>func>>
# テスト
  test # テスト2
# >>func
read_jma <- function(csv_f) {
      data_file
  # 空行削除
# >> 2. ヘッダ header >>func>>
# >>func
        func_2
        test

# <<< func : proc >>>

# >> 1. データ読み込み >>proc>>
# テスト
  test # テスト2
# >>proc
data_tbl <- read_jma(data_file)

# >> 2. ヘッダ header >>proc>>
# >>proc
        proc_2
                testetst
EOS



class SeperateTest <Minitest::Test
        def test_sep_class_1
          testCD = CodeSeperater.new
          assert_equal $result1, testCD.fromChunked($test1)
        end
        def test_sep_class_2
          testCD = CodeSeperater.new
          assert_equal $result2, testCD.fromChunked($test2)
        end
        def test_sep_class_3
          testCD = CodeSeperater.new
          assert_equal $result3, testCD.fromChunked($test3)
        end
        def test_sep_class_4
          testCD = CodeSeperater.new
          assert_equal $result4, testCD.fromChunked($test4)
        end
        def test_sep_class_5
          testCD = CodeSeperater.new
          assert_equal $result5, testCD.fromChunked($test5)
        end
        def test_sep_class_6
          testCD = CodeSeperater.new
          assert_equal $result6, testCD.fromChunked($test6)
        end
        def test_sep_class_7
          testCD = CodeSeperater.new
          assert_equal $result7, testCD.toSeperated($test6)
        end
        def test_sep_class_8a
          testCD = CodeSeperater.new
          assert_equal $result8_a, testCD.fromChunked($test8)
        end
        def test_sep_class_8b
          testCD = CodeSeperater.new
          assert_equal $result8_b, testCD.toSeperated($test8)
        end
      
end