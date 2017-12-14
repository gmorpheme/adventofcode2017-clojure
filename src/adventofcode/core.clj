(ns adventofcode.core
  "Clojure adventofcode.com solutions"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

;;; Prologue

;; Common requirements get refactored up to here when they're needed
;; repeatedly. Avoiding all deps apart from pure Clojure 1.9 for
;; this.

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
        [x y] (first (drop ring-item (ring-seq index)))]
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
    (.indexOf banks m)))

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

(declare total-weight)

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

(defn alt
  "Attempt to parse with `p1` but if that fails `p2`. Fail if both fail."
  [p1 p2]
  (fn [text]
    (if-let [[r1 t1] (p1 text)]
      [r1 t1]
      (when-let [[r2 t2] (p2 text)]
        [r2 t2]))))

(defn opt
  "Attempt parse with `p`, succeeding returning nil if `p` fails."
  [p]
  (fn [text]
    (if-let [[r t] (p text)]
      [r t]
      [nil text])))

(defn fmap
  "Parse with `p` but map return with `f`"
  [p f]
  (fn [text]
    (when-let [[r t] (p text)]
      [(f r) t])))

(defn eat
  "Parse with `p` but return nil result"
  [p]
  (fmap p (constantly nil)))

(defn sq>
  "As per `sq` but return result of last parser, not the sequence."
  [& ps]
  (fmap (apply sq ps) last))

(defn many-sep-by
  "Parse a sequence of `p` separated by `sep`"
  [sep p]
  (opt (fmap
        (sq p (many (sq> sep p)))
        #(apply cons %))))

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

(def group-nesting (fmap (sq (lit \{) (many-sep-by (lit \,) item) (lit \}))
                         (fn [[_ xs _]] (vec (remove nil? xs)))))

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

(defn counted-garbage [] (fmap garbage (fn [[_ cs _]] (count  cs))))

(declare group-9b)

(defn item-9b [text] ((alt (counted-garbage) (group-9b)) text))

(defn group-9b [] (fmap (sq (lit \{) (many-sep-by (lit \,) item-9b) (lit \}))
                      (fn [[_ xs _]] (apply + (remove nil? xs)))))

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
    (->> (map-indexed vector indices)
         (reduce (fn [v [n i]] (assoc v i n)) init))))

(defn run-10a [n lengths]
  (let [indices (:indices (reduce step (start-state n) lengths))]
    (as-list indices)))

(def day10-input [187 254 0 81 169 219 1 190 19 102 255 56 46 32 2 216])

(defn day10a-result []
  (let [[a b & cs] (run-10a 256 day10-input)]
    (* a b)))

(defn lengths-10b [s]
  {:pre [(string? s)]}
  (concat (seq (.getBytes s)) [17 31 73 47 23]))

(defn sparse-to-dense [arr]
  (->> (partition 16 arr)
       (map (partial apply bit-xor))
       (map (partial format "%02x"))
       (apply str)))

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

(defn step-distance [[x y]]
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
  [h]
  (let [binary-rep (.toString  (BigInteger. h 16) 2)
        pad (- 128 (count binary-rep))]
    (str (apply str (repeat pad \0))
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
  (= (get-in bitrows [y x]) \1))

(defn used-neighbours [bitrows cell]
  (filter #(used-cell? % bitrows) (direct-neighbours cell)))

(defn used-cells [bitrows]
  (let [cells (for [x 128 y 128] [x y])]
    (filter #(used-cell? % bitrows) cells)))

(defn day14-bitrows []
  (->> (row-hashes day14-input)
       (map hash-bits)
       (map vec)
       (vec)))

(defn day14b-result []
  (let [bitrows (day14-bitrows)]
    (count (all-components (partial used-neighbours bitrows) (used-cells bitrows)))))

;;; Main

(defn -main []
  (doseq [day (range 1 26)]
    (when-let [a (resolve (symbol (str "day" day "a-result")))]
      (println "Day" day "part a:" (a)))
    (when-let [b (resolve (symbol (str "day" day "b-result")))]
      (println "Day" day "part b:" (b)))))
