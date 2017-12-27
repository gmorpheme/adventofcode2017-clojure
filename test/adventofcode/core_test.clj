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
  (are [x y] (= (cell-distance x) y)
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
  (are [text hash] (= (knot-hash 256 text) hash)
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

(def day14a-expected (-> "##.#.#..
.#.#.#.#
....#.#.
#.#.##.#
.##.#...
##..#..#
.#...#..
##.#.##."
                         (str/replace #"#" "1")
                         (str/replace #"\." "0")
                         (str/split-lines)))

(deftest test-day14a
  (let [subsquare (mapv #(apply str %) (take 8 (map (partial take 8) (map hash-bits (row-hashes "flqrgnkx")))))]
    (is (= subsquare day14a-expected))))

(deftest test-day14b
  (let [bitrows (vec (map vec (map hash-bits (row-hashes "flqrgnkx"))))
        regions (all-components (partial used-neighbours bitrows) (used-cells bitrows))]
    (is (= (count regions) 1242))))

(deftest test-day15a
  (let [day15-test-sequence-a (generator-sequence (generator 16807 2147483647) 65)
        day15-test-sequence-b (generator-sequence (generator 48271 2147483647) 8921)]
    (is (= (take 5 day15-test-sequence-a) [1092455 1181022009 245556042 1744312007 1352636452]))
    (is (= (take 5 day15-test-sequence-b) [430625591 1233683848 1431495498 137874439 285222916]))
    (is (= (take 5 (map generator-match day15-test-sequence-a day15-test-sequence-b)) [false false true false false]))))

(deftest test-day15b
  (let [sequence-a (multiples-of 4 (generator-sequence (generator 16807 2147483647) 65))
        sequence-b (multiples-of 8 (generator-sequence (generator 48271 2147483647) 8921))]
    (is (= (take 5 sequence-a) [1352636452 1992081072 530830436 1980017072 740335192]))
    (is (= (take 5 sequence-b) [1233683848 862516352 1159784568 1616057672 412269392]))

    (testing "first match"
      (is (= (count (drop-while false? (map generator-match sequence-a sequence-b))) 1055)))))

(deftest test-day15b-total-in-5m
  (let [sequence-a (fn [] (multiples-of 4 (generator-sequence (generator 16807 2147483647) 65)))
        sequence-b (fn [] (multiples-of 8 (generator-sequence (generator 48271 2147483647) 8921)))]

    (testing "matches in first 5m"
      (is (= (count (filter true? (take 5000000 (map generator-match (sequence-a) (sequence-b))))) 309)))))

(deftest test-day16-parse
  (are [text inst] (= (run-parse (dance-move) text) inst)
    "s3" [:spin 3]
    "x1/2" [:exchange 1 2]
    "pe/b" [:partner \e \b]))

(deftest test-day16a
  (let [arr (->DanceArray (vec "abcde") 0)]
    (is (= (str (run-dance arr [[:spin 1] [:exchange 3 4] [:partner \e \b]]))
           "baedc"))))

(deftest test-day17a
  (let [spinlock (init-spinlock 3)]
    (is (= (map :base-list (take 10 (iterate step-spinlock spinlock)))
           ['(0)
            '(0 1)
            '(0 2 1)
            '(0 2 3 1)
            '(0 2 4 3 1)
            '(0 5 2 4 3 1)
            '(0 5 2 4 3 6 1)
            '(0 5 7 2 4 3 6 1)
            '(0 5 7 2 4 3 8 6 1)
            '(0 9 5 7 2 4 3 8 6 1)]))

    (is (pos? (java.util.Collections/indexOfSubList
               (:base-list (first (drop 2017 (iterate step-spinlock spinlock))))
               [1512 1134 151 2017 638 1513 851])))))

(def day18-test-input "set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2")

(deftest test-day18a
  (let [pgm (parse-sound-program day18-test-input)]
    (is (= (:recv (first (filter #(contains? % :recv) (sound-program-state-seq pgm))))
           4))))

(def day18b-test-input "snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d")

(deftest test-day18b
  (let [pgm (parse-send-program day18b-test-input)]
    (is (= (execute-day18b-programs pgm) 3))))


(def day19-test-input "     |
     |  +--+
     A  |  C
 F---|----E|--+
     |  |  |  D
     +B-+  +--+ ")

(deftest test-day19a
  (is (= (first (traverse-grid (process-corners (parse-grid-map day19-test-input))))
         "ABCDEF")))

(deftest test-day19b
  (is (= (second (traverse-grid (process-corners (parse-grid-map day19-test-input))))
         38)))

(def day20a-test-particles "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>
p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>")

(deftest test-day20a
  (is (= (:index (min-accel-particle (parse-particle-descriptions day20a-test-particles))) 0)))

(def day20b-test-particles "p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>
p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>
p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>
p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>")

(deftest test-day21a
  (let [rules (parse-art-rules "../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#")]
    (is (= (count-set (nth (iterate (partial art-step rules) genesis-grid) 2)) 12))))

(deftest test-day22a
  (let [init {:infected (read-infection "..#
#..
...") :pos [0 0] :orientation :n :infection-events 0}]

    (is (= (:infected (nth (run-virus init) 1)) #{[0 0] [-1 0] [1 1]}))
    (is (= (:infected (nth (run-virus init) 2)) #{[0 0] [1 1]}))
    (is (= (:infected (nth (run-virus init) 6)) #{[0 0] [-1 0] [-2 0] [-2 1] [-1 1] [1 1]}))
    (is (= (:infected (nth (run-virus init) 7)) #{[0 0] [-1 0] [-2 0] [-2 1] [1 1]}))
    (is (= (:infection-events (nth (run-virus init) 7)) 5))
    (is (= (:infection-events (nth (run-virus init) 70)) 41))
    (is (= (:infection-events (nth (run-virus init) 10000)) 5587))))

(deftest test-day22b
  (let [init {:infected (read-infection "..#
#..
...")
              :weakened #{}
              :flagged #{}
              :pos [0 0]
              :orientation :n
              :infection-events 0}]

    (is (= (:infection-events (nth (run-virus-b init) 100)) 26))
    (is (= (:infection-events (nth (run-virus-b init) 10000000)) 2511944))))

(def day24-test-pieces "0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10")

(deftest test-day24a
  (let [inv (read-inventory day24-test-pieces)]
    (is (= (best-bridges bridge-strength 0 inv)
           [[[0 1] [10 1] [9 10]]]))))

(deftest test-day24b
  (let [inv (read-inventory day24-test-pieces)]
    (is (= (best-bridges (juxt count bridge-strength) 0 inv)
           [[[0 2] [2 2] [2 3] [3 5]]]))))

(def test-day25-turing-def "Begin in state A.
Perform a diagnostic checksum after 6 steps.

In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state B.

In state B:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state A.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A.
")

(deftest test-day25a
  (let [{:keys [start-state check-step machine]} (day25-input test-day25-turing-def)]
    (is (= (nth (map (comp clojure.zip/root first) (turing-machine-sequence machine start-state)) check-step)
           [1 1 0 1]))))
