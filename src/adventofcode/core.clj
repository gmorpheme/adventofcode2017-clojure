(ns adventofcode.core
  "Clojure adventofcode.com solutions"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.zip :as z]
            [clojure.core.async :refer (>!! <!! chan thread timeout alts!!)]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* false)

;;; Todos

;; - Speed up day 5 part b
;; - Speed up day 15
;; - Remove pointless protocol in day 16
;; - Optimise away some stack depth in the parser combinators for day 9 / 16
;; - Add some specs

;;; Prologue

;; Common requirements get refactored up to here when they're needed
;; repeatedly. Avoiding all deps apart from pure Clojure 1.9 for this.

(defn input-text
  "Read input for a day from resources"
  [day]
  (slurp (io/resource (str "day" day ".txt"))))

;;; Day 1

(def day1-input "29917128875332952564321392569634257121244516819997569284938677239676779378822158323549832814412597817651244117851771257438674567254146559419528411463781241159837576747416543451994579655175322397355255587935456185669334559882554936642122347526466965746273596321419312386992922582836979771421518356285534285825212798113159911272923448284681544657616654285632235958355867722479252256292311384799669645293812691169936746744856227797779513997329663235176153745581296191298956836998758194274865327383988992499115472925731787228592624911829221985925935268785757854569131538763133427434848767475989173579655375125972435359317237712667658828722623837448758528395981635746922144957695238318954845799697142491972626942976788997427135797297649149849739186827185775786254552866371729489943881272817466129271912247236569141713377483469323737384967871876982476485658337183881519295728697121462266226452265259877781881868585356333494916519693683238733823362353424927852348119426673294798416314637799636344448941782774113142925315947664869341363354235389597893211532745789957591898692253157726576488811769461354938575527273474399545366389515353657644736458182565245181653996192644851687269744491856672563885457872883368415631469696994757636288575816146927747179133188841148212825453859269643736199836818121559198563122442483528316837885842696283932779475955796132242682934853291737434482287486978566652161245555856779844813283979453489221189332412315117573259531352875384444264457373153263878999332444178577127433891164266387721116357278222665798584824336957648454426665495982221179382794158366894875864761266695773155813823291684611617853255857774422185987921219618596814446229556938354417164971795294741898631698578989231245376826359179266783767935932788845143542293569863998773276365886375624694329228686284863341465994571635379257258559894197638117333711626435669415976255967412994139131385751822134927578932521461677534945328228131973291962134523589491173343648964449149716696761218423314765168285342711137126239639867897341514131244859826663281981251614843274762372382114258543828157464392")

;; Use some partition and apply magic to consider each overlapping
;; pair one by one

(defn day1 [value]
  {:pre [(string? value)]
   :post [(number? %)]}
  (->> value
       (cycle)
       (partition 2 1)
       (take (count value))
       (filter (partial apply =))
       (map #(Integer/parseInt (str (first %))))
       (reduce +)))

(defn day1a-result []
  (day1 day1-input))

;; This time we need to zip together the sequence with an offset of
;; itself. Some transducers just for fun...

(defn day1b [value]
  {:pre [(string? value) (even? (count value))]
   :post [(number? %)]}
  (let [stream (cycle value)
        shifted (drop (/ (count value) 2) stream)
        zipped (map vector stream shifted)]
    (transduce
     (comp (take (count value))
           (filter (partial apply =))
           (map #(Integer/parseInt (str (first %)))))
     +
     zipped)))

(defn day1b-result []
  (day1b day1-input))

;;; Day 2

(defn row-seq
  "Convert string table into seq of seqs reprenting rows and cells"
  [value]
  {:pre [(string? value)]}
  (for [line (str/split-lines value)]
    (map #(Integer/parseInt %) (str/split line #"\s+"))))

(defn day2 [value]
  {:pre [(string? value)]}
  (apply +
         (for [cells (row-seq value)]
           (let [bottom (apply min cells)
                 top (apply max cells)]
             (- top bottom)))))

(defn day2a-result []
  (day2 (input-text 2)))

(defn line-divisor
  "For a set of cells identify the pair where one divides the other and
   return the divisor."
  [cells]
  (let [divisor (fn [vals]
                  (let [head (first vals)
                        tail (rest vals)
                        multiples (filter #(zero? (mod % head)) tail)]
                    (if (empty? multiples)
                      (recur tail)
                      (/ (first multiples) head))))]
    (divisor (apply sorted-set cells))))

(defn day2b [value]
  {:pre [(string? value)]}
  (apply + (map #(line-divisor (apply sorted-set %)) (row-seq value))))

(defn day2b-result []
  (day2b (input-text 2)))


;;; Day 3

;; Part a - do some arithmetic to work out which "ring" the target
;; belongs in then just walk around the ring to find it

(defn ring-index
  "Work out which ring a value is in"
  [value]
  (Math/floor (/  (Math/ceil (Math/sqrt value)) 2)))

(defn ring-min
  "The minimum value in a ring"
  [index]
  (if (zero? index)
    1
    (int (inc (Math/pow (inc (* 2 (dec index))) 2)))))

(defn ring-seq
  "Enumerate the co-ordinates in a ring, starting at the min value and working up to max"
  [index]
  (if (zero? index)
    [[0 0]]
    (concat
     (for [y (range (inc (- index)) (inc index))]
       [index y])
     (for [x (range (dec index) (dec (- index)) -1)]
       [x index])
     (for [y (range (dec index) (dec (- index)) -1)]
       [(- index) y])
     (for [x (range (inc (- index)) (inc index))]
       [x (- index)]))))

(defn cell-distance [value]
  (let [index (ring-index value)
        ring-item (- value (ring-min index))
        [^long x ^long y] (first (drop ring-item (ring-seq index)))]
    (+ (int (Math/abs x)) (int (Math/abs y)))))

(defn day3a-result []
  (cell-distance 368078))

;;; Part b - simulate

(defn spiral-coords
  "Lazy sequence of co-ordinates spiralling out from the origin"
  []
  (for [ring (range)
        c (ring-seq ring)]
    c))

(defn neighbours
  "Return the neighbours of the specified cell"
  [[x y]]
  (for [[dx dy] [[-1  1] [0  1] [1  1]
                 [-1  0]        [1  0]
                 [-1 -1] [0 -1] [1 -1]]]
    [(+ x dx) (+ y dy)]))

(defn allocate
  "From pair of map a last calculated value, produce pair of map with
  next cell filled and the value that was filled in"
  [[grid-map last-val] coordinate]
  (if (= coordinate [0 0])
    [{coordinate 1} 1]
    (let [nvals (map #(get grid-map % 0) (neighbours coordinate))
          total (apply + nvals)]
      [(assoc grid-map coordinate total) total])))

(defn allocations
  "The sequence of states of the grid and values calculated."
  []
  (drop 1 (reductions allocate [{} 0] (spiral-coords))))

(defn cell-values []
  (map second (allocations)))

(defn day3b [value]
  (first (drop-while #(< % value) (cell-values))))

(defn day3b-result []
  (day3b 368078))

;;; Day 4

;; Just populate set of normalised word and count the normalisations

(defn load-passphrases []
  (->> (str/split-lines (input-text 4))
       (map #(str/split % #"\s+"))
       (filter seq)))

(defn valid-passphrase? [words]
  (= (count words) (count (apply hash-set words))))

(defn day4a-result []
  (count (filter valid-passphrase? (load-passphrases))))

(defn valid-passphrase-part2? [words]
  (let [normalised (set (map frequencies words))]
    (= (count words) (count normalised))))

(defn day4b-result []
  (count (filter valid-passphrase-part2? (load-passphrases))))

;;; Day 5

;; Uses explicit state. Also really slow.

(defn process [state index]
  (when-let [jump (get @state index)]
    (vswap! state update index inc)
    jump))

(defn run-day5 [state]
  (loop [count 0 p 0]
    (if-let [jump (process state p)]
      (recur (inc count) (+ jump p))
      count)))

(defn load-instructions []
  (->> (str/split-lines (input-text 5))
       (map #(Integer/parseInt %))
       vec))

(defn day5a-result []
  (run-day5 (volatile! (load-instructions))))

(defn process-b [state index]
  (when-let [jump (get @state index)]
    (let [delta (if (>= jump 3) -1 1)]
      (vswap! state update index #(+ % delta)))
    jump))

(defn run-day5b [state]
  (loop [count 0 p 0]
    (if-let [jump (process-b state p)]
      (recur (inc count) (+ jump p))
      count)))

(defn day5b-result []
  (run-day5b (volatile! (load-instructions))))

;;; Day 6

(defn max-index
  "Find the bank with the most blocks"
  [banks]
  (let [m (apply max banks)]
    (.indexOf ^java.util.List banks m)))

(defn reallocate
  "Do a single reallocation; find largest bank and distribute"
  [banks]
  {:pre [(vector? banks)]}
  (let [bank (max-index banks)
        blocks (get banks bank)
        banks (assoc banks bank 0)
        target-indices (take blocks (drop (inc bank) (cycle (range (count banks)))))]
    (reduce #(update %1 %2 inc) banks target-indices)))

(defn run-reallocate
  [banks]
  (->> (iterate reallocate banks)                      ; successive memory arrangements
       (reductions conj #{})                           ; total set of arrangements seen at each step
       (map-indexed (fn [idx seen] [idx (count seen)])) ; cardinality of seen-set for each time
       (drop-while (fn [[idx count]] (= idx count)))    ; skip to times where the runtime exceeds cardinality
       (first)                                         ; first time we don't increase seen-set by reallocating
       (second)))                                      ; size of the set

(def day6-input [4 10 4 1 8 4 9 14 5 1 14 15 0 15 3 5])

(defn day6a-result []
  (run-reallocate day6-input))

(defn day6b-result []
  (let [state (nth (iterate reallocate day6-input) (day6a-result))]
    (run-reallocate state)))

;;; Day 7

;; Part a is just set difference of programs and subprograms

(defn parse-line [line]
  (let [re #"^(\w+)\s+\((\d+)\)(?:\s+->\s+(.*))?$"]
    (when-let [matches (re-matches re line)]
      (when-let [[name weight suffix] (rest matches)]
        {:name name
         :weight (Integer/parseInt weight)
         :subprograms (when-not (str/blank? suffix) (str/split suffix #",\s+"))}))))

(defn day7-input []
  (->> (str/split-lines (input-text 7))
       (map parse-line)
       (keep identity)))

(defn day7a [input]
  (let [programs (set (map :name input))
        subprograms (set (mapcat :subprograms input))]
    (set/difference programs subprograms)))

;; Part b - memoized recursion to deliver total weight of any
;; program, then search for subprograms that don't balance and go as
;; deep as possible to find the appropriate place to correct

(defn day7a-result []
  (first (day7a (day7-input))))

(defn graph
  "Index by name of program"
  [input]
  (into {} (map (fn [x] [(:name x) x]) input)))

(declare total-weight sender)

(defn total-weight* [{:keys [name weight subprograms]} graph]
  (apply + weight (map #(total-weight (get graph %) graph) subprograms)))

(def total-weight (memoize total-weight*))

(defn find-error
  [graph program]
  {:pre [(string? program)]}
  (let [{:keys [weight subprograms] :as node} (get graph program)
        subweights (into {} (map vector subprograms (map #(total-weight (get graph %) graph) subprograms)))
        freqs (frequencies (vals subweights))]
    (when (> (count freqs) 1)
      ;; assume one subprogram with odd weight
      (let [odd-weight (ffirst (filter (fn [[k v]] (= v 1)) freqs))
            normal-weight (ffirst (filter (fn [[k v]] (> v 1)) freqs))
            odd-program (ffirst (filter (fn [[k v]] (= v odd-weight)) subweights))]

        (or
         ;; try and find a responsible subprogram
         (find-error graph odd-program)
         ;; calculate a weight adjustment for the odd program
         [odd-program (+ (:weight (get graph odd-program)) (- normal-weight odd-weight))])))))

(defn day7b-result []
  (find-error (graph (day7-input)) (day7a-result)))

;;; Day 8

;; Part a - micro DSL to parse the instructions into "command"
;; functions which can be run against a map "environment"

(defn parse-instruction
  [line]
  (letfn [(reg [s] (keyword s))
          (command [register op] #(update % register (fnil op 0)))
          (function [s i] (case s "inc" #(+ % i) "dec" #(- % i)))
          (guard [reg cmp i] #(cmp (get % reg 0) i))
          (comparison [s] (case s "<" < ">" > "==" = "<=" <= ">=" >= "!=" not=))
          (arg [s] (Integer/parseInt s))
          (instruction [c g] #(if (g %) (c %) %))]
    (let [re #"^(\w+)\s+(\w+)\s+(-?\d+)\s+if\s+(\w+)\s+(\S+)\s+(-?\d+)$"
          gs (rest (re-matches re line))]
      (instruction (command (reg (nth gs 0))
                            (function (nth gs 1) (arg (nth gs 2))))
                   (guard (reg (nth gs 3))
                          (comparison (nth gs 4))
                          (arg (nth gs 5)))))))

(defn read-program [s]
  (map parse-instruction (str/split-lines s)))

(def day8-program (read-program (input-text 8)))

(defn run-program [instructions]
  (reduce #(%2 %1) {} instructions))

(defn day8a-result []
  (apply max (vals (run-program day8-program))))

;; Part b - just patch each instruction to keep a :max key in the
;; environment and run again

(defn instrumented [x]
  #(let [state (x %)
         m (reduce max 0 (vals state))]
     (assoc state :max m)))

(defn instrument-program [program]
  (map instrumented program))

(defn day8b-result []
  (:max (run-program (instrument-program day8-program))))

;;; Day 9

;; Implement a few parser combinator primitives - they may come in
;; handy through the month...

(defn ch
  "Parse and return any char"
  [text]
  (when-let [c (first text)] [c (rest text)]))

(defn satisfying
  "Call parser `p` but fail if the result doesn't satisfy predicate `f`"
  [p f]
  (fn [text]
    (let [[r t] (p text)]
      (when (f r) [r t]))))

(defn lit
  "Read and return the character `c` from the text"
  [c]
  (satisfying ch #(= % c)))

(defn sq
  "Parse each of `ps` in sequence, fail if any fail"
  [& ps]
  (fn [text]
    (if (empty? ps)
      [nil text]
      (when-let [[r1 t1] ((first ps) text)]
        (if (seq ps)
          (when-let [[rs t2] ((apply sq (rest ps)) t1)]
            [(cons r1 rs) t2])
          [nil t1])))))

(defn many
  "Parse 0 or more times with `p`, returning list of results"
  [p]
  (fn [text]
    (if-let [[r1 t1] (p text)]
      (if-let [[rs t2] ((many p) t1)]
        [(cons r1 rs) t2]
        [(list r1) t1])
      [nil text])))

(defn many1
  "Parse 1 or more times with `p`, returning list of results"
  [p]
  (fn [text]
    (when-let [[r1 t1] (p text)]
      (if-let [[rs t2] ((many p) t1)]
        [(cons r1 rs) t2]
        [(list r1) t1]))))

(defn alt
  "Attempt to parse with `ps` using result of first that succeeds, else
  fail"
  [& ps]
  (fn [text]
    (when (seq ps)
      (if-let [[r1 t1] ((first ps) text)]
        [r1 t1]
        ((apply alt (rest ps)) text)))))

(defn opt
  "Attempt parse with `p`, succeeding returning nil if `p` fails."
  [p]
  (fn [text]
    (if-let [[r t] (p text)]
      [r t]
      [nil text])))

(defn fmap
  "Parse with `p` but map return with `f`"
  [f p]
  (fn [text]
    (when-let [[r t] (p text)]
      [(f r) t])))

(defn eat
  "Parse with `p` but return nil result"
  [p]
  (fmap (constantly nil) p))

(defn sq>
  "As per `sq` but return result of last parser, not the sequence."
  [& ps]
  (fmap last (apply sq ps)))

(defn many-sep-by
  "Parse a sequence of `p` separated by `sep`"
  [sep p]
  (opt (fmap
        #(apply cons %)
        (sq p (many (sq> sep p))))))

(defn run-parse [parser text]
  (if-let [[result residue] (parser text)]
    (if (empty? residue)
      result
      (throw (ex-info "Incomplete parse" {:residue residue})))
    (throw (ex-info "Parse failure" {}))))

;; deal with !s as preprocess

(defn clean-cancellations [text]
  (-> text
      (str/replace #"!." "")
      (str/trim-newline)))

;; 9a - parse into nested vector structure

(def garbage (sq (lit \<) (many (satisfying ch #(not= % \>))) (lit \>)))

(def eat-garbage (eat garbage))

(declare group-nesting)

(defn item [text] ((alt eat-garbage group-nesting) text))

(def group-nesting (fmap (fn [[_ xs _]] (vec (remove nil? xs)))
                         (sq (lit \{) (many-sep-by (lit \,) item) (lit \}))))

;; ...and functions to evaluate the structure

(defn group-count [groups]
  (inc (apply + (map group-count groups))))

(defn group-score
  ([groups]
   (group-score 1 groups))
  ([depth groups]
   (+ depth (apply + (map (partial group-score (inc depth)) groups)))))

(def day9-input (input-text 9))

(defn day9a-result [] (group-score (run-parse group-nesting (clean-cancellations day9-input))))


;; 9b - alter semantic actions to count garbage instead

(defn counted-garbage [] (fmap (fn [[_ cs _]] (count  cs))garbage))

(declare group-9b)

(defn item-9b [text] ((alt (counted-garbage) (group-9b)) text))

(defn group-9b [] (fmap (fn [[_ xs _]] (apply + (remove nil? xs)))
                        (sq (lit \{) (many-sep-by (lit \,) item-9b) (lit \}))))

(defn day9b-result [] (run-parse (group-9b) (clean-cancellations day9-input)))

;;; Day 10

;; Rather than maintaining an array of values and copying them around
;; to reverse chunks, let's use a little trick and keep instead an
;; array where the contents of the ith index describe where i should
;; appear in the real array. Then moving things around becomes a case
;; of doing some arithmetic on the array items rather than copying
;; around.

(defn reflect-indices [current length indices]
  (let [m (count indices)
        pivot (+ current (/ (dec length) 2))
        reflect (fn [i]  (if (or (and (>= i current) (< i (+ current length)))
                                (and (> (+ current length) m) (< i (mod (+ current length) m))))
                          (mod (int (- pivot (- i pivot))) m)
                          i))]
    (map reflect indices)))

(defn start-state [n]
  {:indices (vec (range n))
   :current 0
   :skip 0})

(defn step [{:keys [indices current skip]} length]
  {:indices (reflect-indices current length indices)
   :current (mod (+ current length skip) (count indices))
   :skip (inc skip)})

(defn as-list [indices]
  (let [init (vec (repeat (count indices) 0))]
    (reduce (fn [v [n i]] (assoc v i n)) init (map-indexed vector indices))))

(defn run-10a [n lengths]
  (let [indices (:indices (reduce step (start-state n) lengths))]
    (as-list indices)))

(def day10-input [187 254 0 81 169 219 1 190 19 102 255 56 46 32 2 216])

(defn day10a-result []
  (let [[a b & cs] (run-10a 256 day10-input)]
    (* a b)))

(defn lengths-10b [^String s]
  {:pre [(string? s)]}
  (concat (seq (.getBytes s)) [17 31 73 47 23]))

(defn sparse-to-dense [arr]
  (->> (partition 16 arr)
       (map (partial apply bit-xor))
       (map (partial format "%02x"))
       (str/join)))

(defn knot-hash
  "`n` is size of knotting array, `s` is string to hash"
  ([n s]
   (let [lengths (lengths-10b s)
         run-once (fn [state] (reduce step state lengths))
         sparse (as-list (:indices (first (drop 64 (iterate run-once (start-state n))))))
         dense (sparse-to-dense sparse)]
     dense))
  ([s] (knot-hash 256 s)))

(def day10b-input "187,254,0,81,169,219,1,190,19,102,255,56,46,32,2,216")

(defn day10b-result []
  (knot-hash 256 day10b-input))

;;; Day 11

;; Represent items in the hex grid as the squares in a normal grid for
;; which both co-ordinates are even or both are odd. Then distances
;; can be calculated using arithmetic. Need to be careful to account
;; for the fact that movement east-west is slower than north-south,
;; i.e. [0 0] to [0 2] is two moves (:ne :se), [0 0] to [0 2] is one
;; move (:n).

(defn offset [move]
  (case move
    :s [0 -2]
    :se [1 -1]
    :sw [-1 -1]
    :n [0 2]
    :ne [1 1]
    :nw [-1 1]))

(defn parse-moves [text]
  (map (comp keyword str/trim) (str/split text #",")))

(defn move [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn find-path [moves]
  (reductions move (map offset moves)))

(defn find-destination [moves]
  (last (find-path moves)))

(defn choose-next [[rem-x rem-y]]
  (cond
    (pos? rem-x) (if (pos? rem-y) :ne :se)
    (neg? rem-x) (if (pos? rem-y) :nw :sw)
    (zero? rem-x) (if (pos? rem-y) :n :s)))

(defn step-distance [[^long x ^long y]]
  (let [abs-x (Math/abs x)
        abs-y (Math/abs y)]
    (if (> abs-x abs-y)
      (+ (- abs-x abs-y) abs-y)
      (/ (+ abs-x abs-y) 2))))

(defn day11-input []
  (parse-moves (input-text 11)))

(defn day11a-result []
  (step-distance (find-destination (day11-input))))

(defn day11b-result []
  (reduce max (map step-distance (find-path (day11-input)))))

;;; Day 12

;; Boooorrring....! DFS for connected components

(defn prune-explored [explored todo]
  (->> (reverse todo)
       (drop-while explored)
       (reverse)))

(defn component [g item]
  (loop [explored #{} todo [item]]
    (if-let [todo (seq (prune-explored explored todo))]
      (let [node (last todo)
            children (set (g node))
            unexplored (set/difference children explored)
            todo+ (vec (concat (butlast todo) unexplored))]
        (recur (conj explored node) todo+))
      explored)))

(defn parse-graph [lines]
  (into {} (for [line lines]
             (let [[src suffix] (rest (re-matches #"(\d+) <-> (.*)" line))
                   connections (map str/trim (str/split suffix #","))]
               [(Integer/parseInt src) (map #(Integer/parseInt %) connections)]))))

(defn day12-input []
  (str/split-lines (input-text 12)))

(defn day12a-result []
  (count (component (parse-graph (day12-input)) 0)))

(defn all-components
  ([g]
   (all-components g (keys g)))
  ([children-fn nodes]
   (loop [components #{} ks nodes]
     (if (seq ks)
       (let [[k & ks'] ks]
         (if (and (seq components) ((apply some-fn components) k))
           (recur components ks')
           (recur (conj components (component children-fn k)) ks')))
       components))))

(defn day12b-result []
  (count (all-components (parse-graph (day12-input)))))

;;; Day 13

;; Movement back and forth in a column of n can be represented as
;; cycling through a "phase space" of 2n - 2 (where each location
;; represents both position and direction in the original column).
;; Everything then becomes pretty straightforward modular arithmetic.

(defn phase-modulus [depth]
  {:pre [(pos? depth)]}
  (- (* 2 depth) 2))

(defn depth [phase-modulus]
  {:pre [(pos? phase-modulus)]}
  (/ (+ phase-modulus 2) 2))

(defn severity [layer phase-modulus]
  {:pre [(pos? phase-modulus) (not (neg? layer))]}
  (* layer (depth phase-modulus)))

(defn caught [picosecond phase-modulus]
  {:pre [(pos? phase-modulus)]}
  (zero? (mod picosecond phase-modulus)))

(defn severities [delay phase-modulus-map]
  (map
   (fn [[layer phase-modulus]]
     (if (caught (+ delay layer) phase-modulus)
       (severity layer phase-modulus)
       0))
   phase-modulus-map))

(defn parse-depth-specs [text]
  (for [line (str/split-lines text)]
    (let [[_ layer depth] (re-matches #"(\d+): (\d+)" line)]
      [(Integer/parseInt layer) (Integer/parseInt depth)])))

(defn phase-modulus-specs [depth-specs]
  (map (fn [[k v]] [k (phase-modulus v)]) depth-specs))

(defn day13a-result []
  (let [specs (phase-modulus-specs (parse-depth-specs (input-text 13)))]
    (reduce + (severities 0 specs))))

(defn uncaught [d phase-modulus-specs]
  (every? (fn [[i m]] (not (zero? (mod (+ d i) m)))) phase-modulus-specs))

(defn find-delay [phase-modulus-specs]
  (first (filter #(uncaught % phase-modulus-specs) (range))))

(defn day13b-result []
  ;; NB there is some redundancy in the specs, sorting by modulus puts
  ;; the tighter constraints first to aid shortcutting
  (let [specs (sort-by second (phase-modulus-specs (parse-depth-specs (input-text 13))))]
    (find-delay specs)))

;;; Day 14

;; Part a, just format conversion and counting digits

(defn row-hashes [s]
  (->> (range 128)
       (map #(str s "-" %))
       (map knot-hash)))

(defn hash-bits
  [^String h]
  (let [binary-rep (.toString  (BigInteger. h 16) 2)
        pad (- 128 (count binary-rep))]
    (str (str/join (repeat pad \0))
         binary-rep)))

(def day14-input "hwlqcszp")

(defn day14a-result []
  (transduce
   (comp (map hash-bits)
         (map (fn [bitstring] (count (filter #(= % \1) bitstring)))))
   +
   (row-hashes day14-input)))

;; Part b, Wheel out the DFS from day 10

(defn direct-neighbours
  "Return the non-diagnoal neighbours of the specified cell"
  [[x y]]
  (for [[dx dy] [[0  1] [-1  0] [1  0] [0 -1]]]
    [(+ x dx) (+ y dy)]))

(defn used-cell? [[x y] bitrows]
  {:pre [(vector? bitrows)]}
  (= (get-in bitrows [y x]) \1))

(defn used-neighbours [bitrows cell]
  {:pre [(vector? bitrows)]}
  (filter #(used-cell? % bitrows) (direct-neighbours cell)))

(defn used-cells [bitrows]
  {:pre [(vector? bitrows)]}
  (let [cells (for [x (range 128) y (range 128)] [x y])]
    (filter #(used-cell? % bitrows) cells)))

(defn day14-bitrows []
  (->> (row-hashes day14-input)
       (map hash-bits)
       (map vec)
       (vec)))

(defn day14b-result []
  (let [bitrows (day14-bitrows)]
    (count (all-components (partial used-neighbours bitrows) (used-cells bitrows)))))

;;; Day 15

(defn generator [factor modulus]
  (fn [input]
    (mod (*' input factor) modulus)))

(defn generator-sequence [g seed]
  (drop 1 (iterate g seed)))

(defn multiples-of [n seq]
  (filter #(zero? (mod % n)) seq))

(defn generator-match [a b]
  (= (bit-and a 0xffff)
     (bit-and b 0xffff)))

(defn day15-sequence-a [] (generator-sequence (generator 16807 2147483647) 679))
(defn day15-sequence-b [] (generator-sequence (generator 48271 2147483647) 771))
(defn day15b-sequence-a [] (multiples-of 4 (day15-sequence-a)))
(defn day15b-sequence-b [] (multiples-of 8 (day15-sequence-b)))

(defn day15a-result []
  (count
   (filter true?
           (take 40000000 (map generator-match (day15-sequence-a) (day15-sequence-b))))))

(defn day15b-result []
  (count
   (filter true?
           (take 5000000 (map generator-match (day15b-sequence-a) (day15b-sequence-b))))))

;;; Day 16

;; Use our parser combinators again, to translate text into
;; instruction list

(defn nat []
  (fmap #(Integer/parseInt (str/join %))
        (many1 (satisfying ch #(and % (Character/isDigit ^java.lang.Character %))))))

(defn spin-move []
  (fmap (fn [n] [:spin n])
        (sq> (lit \s) (nat))))

(defn exchange-move []
  (fmap (fn [[_ i _ j]] [:exchange i j])
        (sq (lit \x) (nat) (lit \/) (nat))))

(defn partner-move []
  (fmap (fn [[_ a _ b]][:partner a b])
        (sq (lit \p) ch (lit \/) ch)))

(defn dance-move [] (alt (spin-move) (exchange-move) (partner-move)))

(defn day16a-instructions []
  (map #(run-parse (dance-move) (str/trim %)) (str/split (input-text 16) #",")))

(defprotocol Dance
  (-spin [self n])
  (-exchange [self i j])
  (-partner [self a b]))

(defrecord DanceArray [store origin]
  Dance
  (-spin [self n]
    (->DanceArray store (mod (- origin n) (count store))))
  (-exchange [self i j]
    (let [i' (mod (+ origin i) (count store))
          j' (mod (+ origin j) (count store))
          ic (nth store i')
          jc (nth store j')
          store' (assoc store i' jc j' ic)]
      (->DanceArray store' origin)))
  (-partner [self a b]
    (->DanceArray (mapv #(cond (= % a) b (= % b) a :else %) store) origin))
  Object
  (toString [self]
    (str/join (concat (subvec store origin) (subvec store 0 origin)))))

(defn dance-array [] (->DanceArray (vec "abcdefghijklmnop") 0))

(defn run-dance [array instructions]
  (let [step (fn [array [i & args]]
               (case i
                 :exchange (apply -exchange array args)
                 :spin (apply -spin array args)
                 :partner (apply -partner array args)))]
    (reduce step array instructions)))

(defn day16a-result []
  (str (run-dance (dance-array) (day16a-instructions))))

;; Part b - a billion takes too long, exploit the fact it is periodic

(defn day16b-result []
  (let [instruction (day16a-instructions)
        one-dance #(run-dance % instruction)
        dance-sequence (drop 1 (iterate one-dance (dance-array)))
        a0 (str (first dance-sequence))
        period (second (remove nil? (map-indexed (fn [i a] (when (= (str a) a0) i)) dance-sequence)))
        rem (mod 1000000000 period)]
    (str (first (drop (dec rem) dance-sequence)))))

;;; Day 17

(defrecord SpinLock [step counter position base-list])

(defn init-spinlock [step]
  (->SpinLock step 1 0 '(0)))

(defn step-spinlock [{:keys [step counter position base-list] :as spinlock}]
  (let [new-pos (mod (+ position step) counter)]
    (->SpinLock
     step
     (inc counter)
     (inc new-pos)
     (apply list (concat (take (inc new-pos) base-list) (cons counter (drop (inc new-pos) base-list)))))))

(defn day17a-result []
  (let [buffer ^java.util.List (:base-list (first (drop 2017 (iterate step-spinlock (init-spinlock 343)))))]
    (nth buffer (inc (.indexOf buffer 2017)))))

(defn light-stepper [step]
  (fn [[^long counter ^long position]]
    [(inc counter) (inc (mod (+ position step) counter))]))

(defn final-insert-at-one [step-size limit]
  (let [step (light-stepper step-size)
        pos-seq (take limit (iterate step [1 0]))
        inserts-at-one (map (comp dec first) (filter (fn [[_ p]] (= 1 p)) pos-seq))]
    (last inserts-at-one)))

(defn day17b-result []
  (final-insert-at-one 343 50000000))

;;; Day 18

;; A few more parser primitives to read the program and convert into
;; functions to modify a map environment

(defn lit-str
  "Parse a string literal"
  [s]
  (apply sq (map lit s)))

(def space (lit \space))

;; We're going to build instructions as functions that operate on map
;; environments. Similarly values (which may be literals or register
;; references) may only have a value in the context of an environment,
;; so we'll parse "rvalues" to functions on environments

(def int-numeral (fmap (fn [[n v]] ((if n - +) v)) (sq (opt (lit \-)) (nat))))
(def reg (fmap (fn [c] (fn [env] (get env c 0))) ch))
(def rvalue (alt (fmap constantly int-numeral) reg))

;; Now the functions on environments which correspond to the
;; instructions. Most of these bump a special :ip instruction pointer
;; register. send / sound and recover / receive also use special
;; registers.

(defn binop
  "Create a function which operates on environment `env` by applying
  binary operator `op` to register and result of applying `value-fn`
  to environment and bumping the instruction pointer register."
  [op register value-fn]
  (fn [env]
    (let [val (value-fn env)]
      (-> env
          (update register (fnil op 0 0) val)
          (update :ip (fnil inc 0))))))

(defn setter
  "Create a function which operates on environment `env` by setting
  `register` to result of applying `value-fn` to register and bumping
  the instruction pointer register. "
  [register value-fn]
  (fn [env]
    (let [val (value-fn env)]
      (-> env
          (assoc register val)
          (update :ip (fnil inc 0))))))

(defn sounder
  [freq-fn]
  (fn [env]
    (let [val (freq-fn env)]
      (-> env
       (assoc :send val)
       (update :ip (fnil inc 0))))))

(defn recoverer
  [cond-fn]
  (fn [env]
    (let [val (cond-fn env)]
      (-> env
          (cond-> (not (zero? val))
            (assoc :recv (get env :send)))
          (update :ip (fnil inc 0))))))

(defn conditional-jumper
  [test-fn jump-fn]
  (fn [env]
    (let [test (test-fn env)
          jump (jump-fn env)]
      (if (pos? test)
        (update env :ip (fnil + 0 0) jump)
        (update env :ip (fnil inc 0))))))

;; Parsers for the instructions, returning environment functions as values

(defn setval [] (fmap (fn [[_ _ r _ v]] (setter r v)) (sq (lit-str "set") space ch space rvalue)))
(defn addval [] (fmap (fn [[_ _ r _ v]] (binop + r v)) (sq (lit-str "add") space ch space rvalue)))
(defn mulval [] (fmap (fn [[_ _ r _ v]] (binop * r v)) (sq (lit-str "mul") space ch space rvalue)))
(defn modval [] (fmap (fn [[_ _ r _ v]] (binop mod r v)) (sq (lit-str "mod") space ch space rvalue)))
(defn snd [] (fmap (fn [[_ _ v]] (sounder v)) (sq (lit-str "snd") space rvalue)))
(defn rcv [] (fmap (fn [[_ _ v]] (recoverer v)) (sq (lit-str "rcv") space rvalue)))
(defn jgz [] (fmap (fn [[_ _ t _ j]] (conditional-jumper t j)) (sq (lit-str "jgz") space rvalue space rvalue)))
(defn parse-sound-instruction [] (alt (setval) (addval) (mulval) (modval) (snd) (rcv) (jgz)))

(defn parse-sound-program
  [text]
  (->> text
       (str/split-lines)
       (map (partial run-parse (parse-sound-instruction)))))

(defn day18a-program []
  (parse-sound-program (input-text 18)))

(defn sound-program-state-seq [pgm]
  (let [step (fn [env]
               (let [ip (get env :ip 0)]
                 (when-let [instruction (nth pgm ip)]
                   (instruction env))))]
    (iterate step {})))

(defn day18a-result []
  (->> (day18a-program)
       (sound-program-state-seq)
       (filter #(contains? % :recv))
       (first)
       :recv))

;; Part b - parse snd and rcv to functions which read and write to
;; core.async channels and record in and out traffic.

(defn complete [env]
  (assoc env :complete true))

(defn sender
  [value-fn]
  (fn [env]
    (let [out (get env :out)
          val (value-fn env)]
      (>!! out val)
      (-> env
          (update :sent (fnil conj []) val)
          (update :ip (fnil inc 0))))))

(defn receiver
  [register]
  (fn [env]
    (let [in (get env :in)
          [val ch] (alts!! [in (timeout 1000)])]
      (if (or (not= ch in) (nil? val))
        (complete env)
        (-> env
            (assoc register val)
            (update :received (fnil conj []) val)
            (update :ip (fnil inc 0)))))))

(defn sendval [] (fmap (fn [[_ _ v]] (sender v)) (sq (lit-str "snd") space rvalue)))
(defn recvval [] (fmap (fn [[_ _ r]] (receiver r)) (sq (lit-str "rcv") space ch)))

(defn parse-send-instruction [] (alt (setval) (addval) (mulval) (modval) (sendval) (recvval) (jgz)))

(defn parse-send-program
  [text]
  (->> text
       (str/split-lines)
       (map (partial run-parse (parse-send-instruction)))))

(defn day18b-program []
  (parse-send-program (input-text 18)))

(defn execute-day18b-program [pgm state]
  (let [state (atom state)
        program (get @state \p)
        step (fn [env]
               (let [ip (get env :ip 0)]
                 (let [instruction (nth pgm ip complete)]
                   (update (instruction env) :tick (fnil inc 0)))))]
    (while (not (:complete @state))
      (swap! state step))
    @state))

(defn execute-day18b-programs [pgm]
  (let [a->b (chan 1000)
        b->a (chan 1000)
        state-a {\p 0 :out a->b :in b->a}
        state-b {\p 1 :out b->a :in a->b}
        thread-a (thread (execute-day18b-program pgm state-a))
        thread-b (thread (execute-day18b-program pgm state-b))
        b-end-state (<!! thread-b)
        a-end-state (<!! thread-a)]
    (count (:sent b-end-state))))

(defn day18b-result []
  (execute-day18b-programs (day18b-program)))

;;; Day 19

;; Y increases southwards for this one

(def north-south (fmap (constantly :ns) (lit \|)))
(def east-west (fmap (constantly :ew) (lit \-)))
(def corner (fmap (constantly :corner) (lit \+)))
(def map-space (eat space))
(def letter (satisfying ch #(and (some? %) (Character/isUpperCase ^char %))))

(def map-cell (alt map-space north-south east-west corner letter))
(def map-row (fmap vec (many map-cell)))

(defn parse-grid-map [text]
  (mapv #(run-parse map-row %) (str/split-lines text)))

(defn process-corners
  "Establish what + marks really represent based on surrounding roads."
  [grid]
  (let [width (apply max (map count grid))
        height (count grid)]
    (vec (for [y (range height)]
           (vec (for [x (range width)]
                  (let [cell (get-in grid [y x])
                        east (get-in grid [y (inc x)])
                        west (get-in grid [y (dec x)])
                        north (get-in grid [(dec y) x])
                        south (get-in grid [(inc y) x])]
                    (case cell
                        :corner (cond-> #{}
                                  (or (#{:ew :corner} west) (char? west)) (conj :w)
                                  (or (#{:ew :corner} east) (char? east)) (conj :e)
                                  (or (#{:ns :corner} north) (char? north)) (conj :n)
                                  (or (#{:ns :corner} south) (char? south)) (conj :s))
                        :ns #{:n :s}
                        :ew #{:e :w}
                        cell))))))))

(defn entrance [grid]
  [(.indexOf ^java.util.List (first grid) #{:n :s}) 0])

(defn move-in-map
  "Return new co-ordinate"
  [[x y] direction]
  {:pre [(#{:n :s :e :w} direction)]}
  (case direction
    :n [x (dec y)]
    :s [x (inc y)]
    :e [(inc x) y]
    :w [(dec x) y]))

(defn opposite-direction [dir]
  {:pre [(#{:n :s :e :w} dir)]}
  (case dir :n :s :s :n :e :w :w :e))

(defn move-through-cell
  "Move through `grid` from `[x y]` assuming incoming direction `direction`. Return
[new-cell-coord new-direction letter-or-nil] or nil if done."
  [grid [x y] direction]
  (when-let [cell (get-in grid [y x])]
    (let [from-direction (opposite-direction direction)
          new-direction (if (and (set? cell) (contains? cell from-direction))
                          (first (disj cell from-direction))
                          direction)
          letter (when (char? cell) cell)]
      [(move-in-map [x y] new-direction) new-direction letter])))

(defn traverse-grid
  [grid]
  (loop [[x y] (entrance grid) dir :s letters [] path []]
    (if-let [[[x' y'] new-dir letter] (move-through-cell grid [x y] dir)]
      (recur [x' y'] new-dir (if letter (conj letters letter) letters) (conj path [x y]))
      [(str/join letters) (count path)])))

(defn day19a-result []
  (let [grid (process-corners (parse-grid-map (input-text 19)))]
    (first (traverse-grid grid))))

(defn day19b-result []
  (let [grid (process-corners (parse-grid-map (input-text 19)))]
    (second (traverse-grid grid))))

;;; Day 20

(def vector-quantity
  (fmap (fn [[_ x _ y _ z _]] [x y z])
        (sq (lit \<) int-numeral (lit \,) int-numeral (lit \,) int-numeral (lit \>))))

(def particle
  (fmap (fn [[_ p _ _ v _ _ a]] {:position p :velocity v :acceleration a})
        (sq (lit-str "p=") vector-quantity (lit-str ", ")
            (lit-str "v=") vector-quantity (lit-str ", ")
            (lit-str "a=") vector-quantity)))

(defn parse-particle-descriptions [text]
  (let [particles (map (partial run-parse particle) (str/split-lines text))]
    (map-indexed (fn [i p] (assoc p :index i)) particles)))

(defn day20-particles []
  (parse-particle-descriptions (input-text 20)))

(defn manhattan-magnitude [[^long x ^long y ^long z]]
  (+ (Math/abs x)
     (Math/abs y)
     (Math/abs z)))

(defn euclidean-norm [[x y z]]
  (let [sqr (fn [i] (* i i))]
    (Math/sqrt (+ (sqr x) (sqr y) (sqr z)))))

(defn min-accel-particle [particles]
  (apply min-key (comp manhattan-magnitude :acceleration) particles))

(defn day20a-result []
  (let [particles (day20-particles)]
    (:index (min-accel-particle particles))))

;; We need full simulation for part b because of the discrete nature
;; of the problem... particles don't collide if they hop over each
;; other...

(defn vector+ [[lx ly lz] [rx ry rz]]
  [(+' lx rx) (+' ly ry) (+' lz rz)])

(defn evolve1 [particles]
  (for [{:keys [position velocity acceleration] :as p} particles]
    (let [new-velocity (vector+ velocity acceleration)]
      (assoc p
             :velocity new-velocity
             :position (vector+ position new-velocity)))))

(defn prune-collisions [particles]
  (let [grouped (group-by :position particles)]
    (->> grouped
         (filter (fn [[pos ps]] (= 1 (count ps))))
         (mapcat second))))

(defn evolve [particles]
  (let [step (comp prune-collisions evolve1)]
    (iterate step particles)))

;; Crude termination condition - experimented with equivalent sorts of
;; p, v, a but in the end just looked for a period (1000 repetitions)
;; of stable particle count

(defn day20b-result []
  (let [evolving-count (reductions
                        (fn [[prev-n repeats] ps] (if (= prev-n (count ps)) [prev-n (inc repeats)] [(count ps) 1]))
                        [1000 0]
                        (evolve (day20-particles)))]
    (ffirst (filter (fn [[count repeats]] (> repeats 1000)) evolving-count))))

;;; Day 21

(defprotocol ArtGrid
  (grid-size [self])
  (is-set? [self [x y]]))

(defrecord SimpleGrid [rows]
  ArtGrid
  (grid-size [self]
    (count rows))
  (is-set? [self [x y]]
    (= 1 (get-in rows [y x]))))

(defn simple-grid [rows]
  {:pre [(vector? rows) (vector? (first rows)) (= (count rows) (count (first rows)))]}
  (->SimpleGrid rows))

(defrecord BlockGrid [block-rows block-size]
  ArtGrid
  (grid-size [self]
    (* block-size (count block-rows)))
  (is-set? [self [x y]]
    (let [block (get-in block-rows [(quot y block-size) (quot x block-size)])]
      (is-set? block [(mod x block-size) (mod y block-size)]))))

(defn block-grid [block-rows]
  (let [block-size (grid-size (ffirst block-rows))]
    (->BlockGrid block-rows block-size)))

(def genesis-grid (->SimpleGrid [[0 1 0]
                                 [0 0 1]
                                 [1 1 1]]))

(def art-grid-cell (alt (fmap (constantly 0) (lit \.)) (fmap (constantly 1) (lit \#))))
(def art-grid-row (fmap vec (many art-grid-cell)))
(def art-grid (fmap (comp ->SimpleGrid vec) (many-sep-by (lit \/) art-grid-row)))
(def art-rule (fmap (fn [[match _ sub]] {:match match :substitution sub}) (sq art-grid (lit-str " => ") art-grid)))

(defn grid-identifier
  "Unique id for this grid (within this grid-size) formed by reading the
  elements as a binary number. Least significant digit is top-left, most
  significant is bottom right"
  [grid]
  (reduce bit-or (let [sz (grid-size grid)]
                   (for [y (range sz) x (range sz)]
                     (if (is-set? grid [x y])
                       (bit-shift-left 1 (+ (* y sz) x))
                       0)))))

(defn rotate-clockwise [grid]
  (let [sz (grid-size grid)]
    (simple-grid
     (vec (for [x (range sz)]
            (vec (for [y (range (dec sz) -1 -1)]
                   (if (is-set? grid [x y]) 1 0))))))))

(defn reflect-in-vertical [grid]
  (simple-grid (mapv (comp vec reverse) (:rows grid))))

(defn equivalent-grids
  [grid]
  (set (concat (take 4 (iterate rotate-clockwise grid))
               (take 4 (iterate rotate-clockwise (reflect-in-vertical grid))))))

(defn equivalent-grid-identifiers [grid]
  (map grid-identifier (equivalent-grids grid)))

(defn index-rule [index {:keys [match substitution]}]
  (let [eq-ids (equivalent-grid-identifiers match)]
    (reduce
     #(assoc %1 %2 substitution)
     index
     eq-ids)))

(defn parse-art-rules
  "Parses art rules into lookup vectors, one for two-grids and one for
  three-grids, each storing substitutions grids at indexes
  corresponding to grid-identifiers in the vector."
  [text]
  (let [rules (map (partial run-parse art-rule) (str/split-lines text))
        two-grid-rules (filter #(= 2 (grid-size (:match %))) rules)
        three-grid-rules (filter #(= 3 (grid-size (:match %))) rules)
        index-2 (vec (take 16 (repeat nil)))
        index-3 (vec (take 512 (repeat nil)))]
    [(reduce index-rule index-2 two-grid-rules)
     (reduce index-rule index-3 three-grid-rules)]))

(defn read-grid-identifier
  "Read a grid of size starting at [x y] and return the identifier for it"
  [grid size [x y]]
  (reduce bit-or
          (for [dy (range size) dx (range size)]
            (if (is-set? grid [(+ x dx) (+ y dy)])
              (bit-shift-left 1 (+ (* dy size) dx))
              0))))

(defn block-identifiers
  "Dividing the grid into blocks of `size`, return the rows of
  grid-identifiers of these blocks"
  [grid block-size]
  (let [sz (grid-size grid)]
    (for [y (range 0 sz block-size)]
      (for [x (range 0 sz block-size)]
        (read-grid-identifier grid block-size [x y])))))

(defn art-step [[index-2 index-3] grid]
  (let [sz (grid-size grid)
        even? (zero? (mod sz 2))
        next-size (if even? 2 3)
        index (if even? index-2 index-3)]
    (->> (block-identifiers grid next-size)
         (mapv #(mapv (fn [id] (get index id)) %))
         (block-grid))))

(defn count-set [grid]
  (let [sz (grid-size grid)]
    (count
     (filter identity
             (for [x (range sz) y (range sz)]
               (is-set? grid [x y]))))))

(defn evolve-art []
  (iterate (partial art-step (parse-art-rules (input-text 21))) genesis-grid))

(defn day21a-result []
  (count-set (nth (evolve-art) 5)))

(defn day21b-result []
  (count-set (nth (evolve-art) 18)))

;;; Day 22

;; We won't represent the grid, just track infected cells, so more
;; convenient for Y to increase Northwards

(defn read-infection
  "Read initial map and return set of infected co-ordinates"
  [text]
  (let [grid (->> (str/split-lines text)
                  (mapv #(mapv (fn [c] (case c \# true false)) %)))
        width (count (first grid))
        height (count grid)
        min-x (/ (dec width) 2)
        min-y (/ (dec height) 2)
        range-x (range (- min-x) (inc min-x))
        range-y (range min-y (- (inc min-y)) -1)]
    (set
     (remove nil? (for [y range-y x range-x]
                    (when (get-in grid [(+ min-y (- y)) (+ min-x x)])
                      [x y]))))))


(defn turn-right [orientation]
  (case orientation
    :n :e
    :e :s
    :s :w
    :w :n))

(defn turn-left [orientation]
  (case orientation
    :e :n
    :s :e
    :w :s
    :n :w))

(defn advance [orientation [x y]]
  (case orientation
    :n [x (inc y)]
    :e [(inc x) y]
    :s [x (dec y)]
    :w [(dec x) y]))

(defn step-virus [{:keys [infected pos orientation infection-events]}]
  (if (infected pos)
    (let [new-dir (turn-right orientation)
          infected (disj infected pos)
          new-pos (advance new-dir pos)]
      {:infected infected
       :pos new-pos
       :orientation new-dir
       :infection-events infection-events})
    (let [new-dir (turn-left orientation)
          infected (conj infected pos)
          new-pos (advance new-dir pos)]
      {:infected infected
       :pos new-pos
       :orientation new-dir
       :infection-events (inc infection-events)})))

(defn initial-day22-state []
  {:infected (read-infection (input-text 22))
   :weakened #{}
   :flagged #{}
   :pos [0 0]
   :orientation :n
   :infection-events 0})

(defn run-virus [state]
  (iterate step-virus state))

(defn render
  "Useful for REPL-based diagnostics..."
  [{:keys [infected flagged weakened] :as state}]
  (let [marked (set/union infected flagged weakened)
        min-x (reduce min (map first marked))
        max-x (reduce max (map first marked))
        min-y (reduce min (map second marked))
        max-y (reduce max (map second marked))]
    (doseq [y (range max-y (dec min-y) -1)]
      (doseq [x (range min-x (inc max-x))]
        (cond
          (infected [x y]) (print "#")
          (flagged [x y]) (print "F")
          (weakened [x y]) (print "W")
          :else (print ".")))
      (print \newline))
    (println "Infection events:" (:infection-events state))))

(defn day22a-result []
  (:infection-events (nth (run-virus (initial-day22-state)) 10000)))

;; Part b is just a different step function

(defn step-virus-b [{:keys [infected flagged weakened pos orientation infection-events] :as state}]
  (cond
    (infected pos) (let [infected (disj infected pos)
                         flagged (conj flagged pos)
                         new-dir (turn-right orientation)
                         new-pos (advance new-dir pos)]
                     (assoc state
                            :infected infected
                            :flagged flagged
                            :pos new-pos
                            :orientation new-dir))
    (flagged pos) (let [flagged (disj flagged pos)
                        new-dir (turn-right (turn-right orientation))
                        new-pos (advance new-dir pos)]
                    (assoc state
                           :flagged flagged
                           :pos new-pos
                           :orientation new-dir))
    (weakened pos) (let [infected (conj infected pos)
                         weakened (disj weakened pos)
                         new-pos (advance orientation pos)]
                     (assoc state
                            :infected infected
                            :weakened weakened
                            :pos new-pos
                            :infection-events (inc infection-events)))
    :else (let [weakened (conj weakened pos)
                new-dir (turn-left orientation)
                new-pos (advance new-dir pos)]
            (assoc state
                   :weakened weakened
                   :pos new-pos
                   :orientation new-dir))))

(defn run-virus-b [state]
  (iterate step-virus-b state))

(defn day22b-result []
  (:infection-events (nth (run-virus-b (initial-day22-state)) 10000000)))

;;; Day 23

;; Re-use most of the day 18 parsers and implementation with new op code

(defn non-zero-jumper
  [test-fn jump-fn]
  (fn [env]
    (let [test (test-fn env)
          jump (jump-fn env)]
      (if (not (zero? test))
        (update env :ip (fnil + 0 0) jump)
        (update env :ip (fnil inc 0))))))

;; And decorator for counting op codes

(defn tally
  "Instrument an op code function to keep track of number of executions
  at specified key in the environment"
  [key op]
  (fn [env]
    (update (op env) key (fnil inc 0))))

(defn tallied-mulval [] (fmap (partial tally :mulval) (mulval)))
(defn subval [] (fmap (fn [[_ _ r _ v]] (binop - r v)) (sq (lit-str "sub") space ch space rvalue)))
(defn jnz [] (fmap (fn [[_ _ t _ j]] (non-zero-jumper t j)) (sq (lit-str "jnz") space rvalue space rvalue)))

(defn parse-day23a-instruction [] (alt (setval) (subval) (tallied-mulval) (jnz)))

(defn parse-day23a-program []
  (map
   (partial run-parse (parse-day23a-instruction))
   (str/split-lines (input-text 23))))

(defn day23-state-seq [pgm initial-state]
  (let [step (fn [env]
               (let [ip (get env :ip 0)]
                 (let [instruction (nth pgm ip complete)]
                   (update (instruction env) :tick (fnil inc 0)))))]
    (iterate step initial-state)))

(defn execute-day23-program [pgm initial-state]
      (first (filter :complete (day23-state-seq pgm initial-state))))

(defn day23a-result []
  (:mulval (execute-day23-program (parse-day23a-program) {})))

;; By inspection the instructions calculate a count of composite
;; numbers as implemented below...

(declare prime?)
(defn prime* [n]
  (let [composite (some true? (for [i (range 2 n) :when (prime? i)] (zero? (mod n i))))]
    (not composite)))
(def prime? (memoize prime*))

(defn find-composites []
  (let [start (+ 100000 (* 93 100))
        end (+ start 17000)
        candidates (range start (inc end) 17)]
    (count (filter (complement prime?) candidates))))

(defn day23b-result []
  (find-composites))

;;; Day 24

(defn read-inventory [text]
  (let [parse-line (fn [line] (->> (re-matches #"(\d+)/(\d+)" line)
                                  (drop 1)
                                  (map #(Integer/parseInt %))
                                  (vec)))]
    (into #{} (map parse-line (str/split-lines text)))))

(defn bridge-strength [pieces]
  (reduce + (map (partial apply +) pieces)))

(defn other-pin [pin piece]
  (if (= (first piece) pin)
    (second piece)
    (first piece)))

(defn matches-pin [pin piece]
  (or (= (first piece) pin)
      (= (second piece) pin)))

(defn maximal-elements
  "Like `max-key` but returns all equal matches"
  [k elts]
  (if-let [[x & xs] elts]
    (reduce (fn [ys x]
              (let [c (compare (k x) (k (peek ys)))]
                (cond
                  (pos? c) [x]
                  (neg? c) ys
                  :else    (conj ys x))))
            [x]
            xs)
    []))

;; Part a compares by strength, part b by length then strength. In
;; each case, however deep in the recursion we are, we can ignore all
;; but the best bridges as incorporating the shared prefix segment
;; into either bridge cannot change the ordering.

(defn best-bridges [comparator pin inventory]
  (let [continue-bridge (fn [next-piece]
                          (let [remaining-pieces (disj inventory next-piece)
                                new-end-pin (other-pin pin next-piece)
                                continuation-bridges (best-bridges comparator new-end-pin remaining-pieces)
                                suffixes (map (partial cons next-piece) continuation-bridges)]
                            (if (empty? suffixes)
                              [[next-piece]]
                              suffixes)))]
    (->> inventory
         (filter (partial matches-pin pin))
         (mapcat continue-bridge)
         (maximal-elements comparator))))

(defn day24a-result []
  (bridge-strength (first (best-bridges bridge-strength 0 (read-inventory (input-text 24))))))

(defn day24b-result []
  (apply max (map bridge-strength (best-bridges (juxt count bridge-strength) 0 (read-inventory (input-text 24))))))

;;; Day 25

;; Parsing this is hardly worth it but should at least save time lost
;; to typos

(def eol (lit \newline))

(def state-header (fmap (fn [[_ s _]] s)
                        (sq (lit-str "In state ") ch (lit \:))))

(def transition-header (fmap (fn [[_ _ i]] [:match i])
                             (sq (many space) (lit-str "If the current value is ") (nat) (lit \:))))

(def transition-write (fmap (fn [[_ _ i _]] [:write i])
                            (sq (many space) (lit-str "- Write the value ") (nat) (lit \.))))

(def transition-move (fmap (fn [[_ _ dir _]] [:move (case (apply str dir) "left" :left "right" :right)])
                           (sq (many space) (lit-str "- Move one slot to the ") (alt (lit-str "left") (lit-str "right")) (lit \.))))

(def transition-continue (fmap (fn [[_ _ s _]] [:next s])
                               (sq (many space) (lit-str "- Continue with state ") ch (lit \.))))

(def state-transition (fmap (fn [[h _ w _ m _ c _]]
                              (into {} [h w m c]))
                            (sq transition-header eol
                                transition-write eol
                                transition-move eol
                                transition-continue eol)))

(def state-recipe (fmap (fn [[s _ ts]] [s (vec ts)])
                        (sq state-header eol (many state-transition))))

(def turing-machine-definition (fmap (fn [recipes] (into {} recipes))
                                     (many-sep-by eol state-recipe)))

(def initial-turing-machine-state (fmap (fn [[_ s _]] [:start-state s])
                                        (sq (lit-str "Begin in state ") ch (lit \.))))

(def turing-machine-check (fmap (fn [[_ n _]] [:check-step n]) (sq (lit-str "Perform a diagnostic checksum after ") (nat) (lit-str " steps."))))

(def day25-file (fmap (fn [[start _ check _ _ machine _]] (into {} [start check [:machine machine]]))
                      (sq initial-turing-machine-state eol
                          turing-machine-check eol eol
                          turing-machine-definition)))


(defn day25-input [text]
  (run-parse day25-file text))

;; A Turing machine - don't need any notion of origin we just need to
;; extend in both directions. Let's use a zipper.

(defn initial-machine-tape []
  (z/next (z/seq-zip '(0))))

(defn tape-left [zipper]
  (if-let [zipper' (z/left zipper)]
    zipper'
    (-> zipper
        (z/insert-left 0)
        (z/left))))

(defn tape-right [zipper]
  (if-let [zipper' (z/right zipper)]
    zipper'
    (-> zipper
        (z/insert-right 0)
        (z/right))))

(defn tape-move [zipper direction]
  (case direction
    :left (tape-left zipper)
    :right (tape-right zipper)))

(defn apply-machine
  "Apply `machine`'s `state` rules to the tape represented by `zipper`"
  [machine state zipper]
  (let [rules (machine state)
        value (z/node zipper)
        rule (first (filter #(= (:match %) value) rules))
        tape (-> zipper
                 (z/replace (:write rule))
                 (tape-move (:move rule)))]
    [tape (:next rule)]))

(defn turing-machine-sequence
  [machine start-state]
   (iterate (fn [[t s]] (apply-machine machine s t)) [(initial-machine-tape) start-state]))

(defn run-turing-machine
  [machine state steps]
  (let [tape (first (nth (turing-machine-sequence machine state) steps))]
    (z/root tape)))

(defn day25a-result []
  (let [{:keys [start-state check-step machine]} (day25-input (input-text 25))]
    (reduce + (run-turing-machine machine start-state check-step))))

;;; Main

(defn -main []
  (doseq [day (range 1 26)]
    (when-let [a (resolve (symbol (str "day" day "a-result")))]
      (println "Day" day "part a:" (a)))
    (when-let [b (resolve (symbol (str "day" day "b-result")))]
      (println "Day" day "part b:" (b)))))
