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
  (is (= (run-day5 (volatile! [0 3 0 1 -3])) 5)))

(deftest test-day5b
  (is (= (run-day5b (volatile! [0 3 0 1 -3])) 10)))


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

(deftest test-day9a-counts
  (are [text n] (= (group-count (run-parse group-nesting (clean-cancellations text))) n)
    "{}" 1
    "{{{}}}" 3
    "{{},{}}" 3
    "{{{},{},{{}}}}" 6
    "{<{},{},{{}}>}" 1
    "{<a>,<a>,<a>,<a>}" 1
    "{{<a>},{<a>},{<a>},{<a>}}" 5
    "{{<!>},{<!>},{<!>},{<a>}}" 2))

(deftest test-day9a-scores
  (are [text n] (= (group-score (run-parse group-nesting (clean-cancellations text))) n)
    "{}" 1
    "{{{}}}" 6
    "{{},{}}" 5
    "{{{},{},{{}}}}" 16
    "{<{},{},{{}}>}" 1
    "{{<ab>},{<ab>},{<ab>},{<ab>}}" 9
    "{{<!!>},{<!!>},{<!!>},{<!!>}}" 9
    "{{<a!>},{<a!>},{<a!>},{<ab>}}" 3))

(deftest test-day9b
  (are [text n] (= (run-parse (counted-garbage) (clean-cancellations text)) n)
    "<>" 0
    "<random characters>" 17
    "<<<<>" 3
    "<{!>}>" 2
    "<!!>" 0
    "<!!!>>" 0
    "<{o\"i!a,<{i<a>" 10))

(deftest test-day10b
  (are [text hash] (= (run-10b 256 text) hash)
    "" "a2582a3a0e66e6e86e3812dcb672a272"
    "AoC 2017" "33efeb34ea91902bb2f59c9920caa6cd"
    "1,2,3" "3efbe78a8d82f29979031a4aa0b16a9d"
    "1,2,4" "63960835bcdc130f0b66d7ff4f6a5a8e"))

(deftest test-day11a
  (are [text step-count] (= (step-distance (find-destination (parse-moves text)))
                            step-count)
    "ne,ne,ne" 3
    "ne,ne,sw,sw" 0
    "ne,ne,s,s" 2
    "se,sw,se,sw,sw" 3))

(deftest test-day12a
  (let [g (parse-graph (str/split-lines "0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5"))]
    (is (= (count (component g 0)) 6))))

(def day13a-test-input "0: 3
1: 2
4: 4
6: 4")

(deftest test-day13a
  (let [depth-specs (parse-depth-specs day13a-test-input)
        moduli (phase-modulus-specs depth-specs)]
    (is (= (reduce + (severities 0 moduli)) 24))))

(deftest test-day13b
  (let [depth-specs (parse-depth-specs day13a-test-input)
        moduli (phase-modulus-specs depth-specs)]
    (is (= (find-delay moduli) 10))))
