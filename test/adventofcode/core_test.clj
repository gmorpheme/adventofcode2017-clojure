(ns adventofcode.core-test
  (:require [clojure.test :refer :all]
            [adventofcode.core :refer :all]
            [clojure.string :as str]))

(deftest test-day1
  (are [x y] (= (day1 x) y)
    "1122" 3
    "1111" 4
    "1234" 0
    "91212129" 9))

(deftest test-day1b
  (are [x y] (= (day1b x) y)
    "1212" 6
    "1122" 0
    "123425" 4
    "123123" 12
    "12131415" 4))

(deftest test-day2
  (is (= (day2 "5 1 9 5\n7 5 3\n2 4 6 8") 18)))

(deftest test-day2b
  (is (= (day2b "5 9 2 8\n9 4 7 3\n3 8 6 5") 9)))

(deftest test-day3a
  (are [x y] (= (day3a x) y)
    1 0
    12 3
    23 2
    1024 31))

(deftest test-day3b-seq
  (is (= (take 23 (cell-values))
         [1 1 2 4 5 10 11 23 25 26 54 57 59 122 133 142 147 304 330 351 362 747 806])))

(deftest test-day4
  (is (valid-passphrase? ["aa" "bb" "cc" "dd" "ee"]))
  (is (not (valid-passphrase? ["aa" "bb" "cc" "dd" "aa"])))
  (is (valid-passphrase? ["aa" "bb" "cc" "dd" "aaa"])))

(deftest test-day4b
  (are [x y] (= (valid-passphrase-part2? (str/split x #"\s")) y)
    "abcde fghij" true
    "abcde xyz ecdab" false
    "a ab abc abd abf abj" true
    "iiii oiii ooii oooi oooo" true
    "oiii ioii iioi iiio" false))

(deftest test-day5a
  (is (= (run-day5 (atom [0 3 0 1 -3])) 5)))

(deftest test-day5b
  (is (= (run-day5b (atom [0 3 0 1 -3])) 10)))
