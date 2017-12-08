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


;;; qq day6

(def day7-test-input (map parse-line (str/split-lines "pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)")))

(deftest test-day7a
  (is (= #{"tknk"} (day7a day7-test-input))))

(deftest test-day7b
  (is (= ["ugml" 60] (find-error (graph day7-test-input) "tknk"))))

(def day8-test-input "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10")

(deftest test-day8a
  (is (= (run-program (read-program day8-test-input)) {:a 1 :c -10})))

(deftest test-day8b
  (is (= (:max (run-program (instrument-program (read-program day8-test-input)))) 10)))
