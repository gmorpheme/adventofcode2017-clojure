(ns adventofcode.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

;;; Day 2

(def day1-input "29917128875332952564321392569634257121244516819997569284938677239676779378822158323549832814412597817651244117851771257438674567254146559419528411463781241159837576747416543451994579655175322397355255587935456185669334559882554936642122347526466965746273596321419312386992922582836979771421518356285534285825212798113159911272923448284681544657616654285632235958355867722479252256292311384799669645293812691169936746744856227797779513997329663235176153745581296191298956836998758194274865327383988992499115472925731787228592624911829221985925935268785757854569131538763133427434848767475989173579655375125972435359317237712667658828722623837448758528395981635746922144957695238318954845799697142491972626942976788997427135797297649149849739186827185775786254552866371729489943881272817466129271912247236569141713377483469323737384967871876982476485658337183881519295728697121462266226452265259877781881868585356333494916519693683238733823362353424927852348119426673294798416314637799636344448941782774113142925315947664869341363354235389597893211532745789957591898692253157726576488811769461354938575527273474399545366389515353657644736458182565245181653996192644851687269744491856672563885457872883368415631469696994757636288575816146927747179133188841148212825453859269643736199836818121559198563122442483528316837885842696283932779475955796132242682934853291737434482287486978566652161245555856779844813283979453489221189332412315117573259531352875384444264457373153263878999332444178577127433891164266387721116357278222665798584824336957648454426665495982221179382794158366894875864761266695773155813823291684611617853255857774422185987921219618596814446229556938354417164971795294741898631698578989231245376826359179266783767935932788845143542293569863998773276365886375624694329228686284863341465994571635379257258559894197638117333711626435669415976255967412994139131385751822134927578932521461677534945328228131973291962134523589491173343648964449149716696761218423314765168285342711137126239639867897341514131244859826663281981251614843274762372382114258543828157464392")

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

(defn day1-result []
  (day1 day1-input))

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


(def day2-input "409	194	207	470	178	454	235	333	511	103	474	293	525	372	408	428
4321	2786	6683	3921	265	262	6206	2207	5712	214	6750	2742	777	5297	3764	167
3536	2675	1298	1069	175	145	706	2614	4067	4377	146	134	1930	3850	213	4151
2169	1050	3705	2424	614	3253	222	3287	3340	2637	61	216	2894	247	3905	214
99	797	80	683	789	92	736	318	103	153	749	631	626	367	110	805
2922	1764	178	3420	3246	3456	73	2668	3518	1524	273	2237	228	1826	182	2312
2304	2058	286	2258	1607	2492	2479	164	171	663	62	144	1195	116	2172	1839
114	170	82	50	158	111	165	164	106	70	178	87	182	101	86	168
121	110	51	122	92	146	13	53	34	112	44	160	56	93	82	98
4682	642	397	5208	136	4766	180	1673	1263	4757	4680	141	4430	1098	188	1451
158	712	1382	170	550	913	191	163	459	1197	1488	1337	900	1182	1018	337
4232	236	3835	3847	3881	4180	4204	4030	220	1268	251	4739	246	3798	1885	3244
169	1928	3305	167	194	3080	2164	192	3073	1848	426	2270	3572	3456	217	3269
140	1005	2063	3048	3742	3361	117	93	2695	1529	120	3480	3061	150	3383	190
489	732	57	75	61	797	266	593	324	475	733	737	113	68	267	141
3858	202	1141	3458	2507	239	199	4400	3713	3980	4170	227	3968	1688	4352	4168
")

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

(defn day2-result []
  (day2 day2-input))

(defn line-divisor
  "For a set of cells identify the pair where one divides the other and
   return the divisor."
  [cells]
  (let [divisor (fn [vals]
                  (let [head (first vals)
                        tail (rest vals)
                        multiples (filter #(= 0 (mod % head)) tail)]
                    (if (empty? multiples)
                      (recur tail)
                      (/ (first multiples) head))))]
    (divisor (apply sorted-set cells))))

(defn day2b [value]
  {:pre [(string? value)]}
  (apply + (map #(line-divisor (apply sorted-set %)) (row-seq value))))

(defn day2b-result []
  (day2b day2-input))


;;; Day 3

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

(def day3a cell-distance)

(defn day3a-result []
  (day3a 368078))

(defn spiral-coords []
  (for [ring (range)
        c (ring-seq ring)]
    c))

(defn neighbours [[x y]]
  (for [[dx dy] [[-1  1] [0  1] [1  1]
                 [-1  0]        [1  0]
                 [-1 -1] [0 -1] [1 -1]]]
    [(+ x dx) (+ y dy)]))

(defn allocate
  "From pair of map a last calculated value, produce pair of map with
  next cell filled and the value villed in"
  [[grid-map last-val] coordinate]
  (if (= coordinate [0 0])
    [{coordinate 1} 1]
    (let [nvals (map #(get grid-map % 0) (neighbours coordinate))
          total (apply + nvals)]
      [(assoc grid-map coordinate total) total])))

(defn allocations []
  (drop 1 (reductions allocate [{} 0] (spiral-coords))))

(defn cell-values []
  (map second (allocations)))

(defn day3b [value]
  (first (drop-while #(< % value) (cell-values))))

(defn day3b-result []
  (day3b 368078))

;;; Day 4

(defn load-passphrases []
  (->> (str/split-lines (slurp (io/resource "day4.txt")))
       (map #(str/split % #"\s+"))
       (filter #(not (empty? %)))))

(defn valid-passphrase? [words]
  (= (count words) (count (apply hash-set words))))

(defn day4-result []
  (count (filter valid-passphrase? (load-passphrases))))

(defn valid-passphrase-part2? [words]
  (let [normalised (into #{} (map frequencies words))]
    (= (count words) (count normalised))))

(defn day4b-result []
  (count (filter valid-passphrase-part2? (load-passphrases))))

;;; Day 5

(defn process [state index]
  (when-let [jump (get @state index)]
    (swap! state update index inc)
    jump))

(defn run-day5 [state]
  (loop [count 0 p 0]
    (if-let [jump (process state p)]
      (recur (inc count) (+ jump p))
      count)))

(defn load-instructions []
  (->> (str/split-lines (slurp (io/resource "day5.txt")))
       (map #(Integer/parseInt %))
       vec))

(defn day5a-result []
  (run-day5 (atom (load-instructions))))

(defn process-b [state index]
  (when-let [jump (get @state index)]
    (let [delta (if (>= jump 3) -1 1)]
      (swap! state update index #(+ % delta)))
    jump))

(defn run-day5b [state]
  (loop [count 0 p 0]
    (if-let [jump (process-b state p)]
      (recur (inc count) (+ jump p))
      count)))

(defn day5b-result []
  (run-day5b (atom (load-instructions))))

;;; Day 6

(defn max-index
  [mem]
  (let [m (apply max mem)]
    (.indexOf mem m)))

(defn reallocate
  "Do a single reallocation; find largest bank and distribute"
  [mem]
  {:pre [(vector? mem)]}
  (let [bank (max-index mem)
        blocks (get mem bank)
        mem (assoc-in mem [bank] 0)
        target-indices (take blocks (drop (inc bank) (cycle (range (count mem)))))]
    (reduce #(update-in %1 [%2] inc) mem target-indices)))

(defn run-reallocate
  [mem]
  (->> (iterate reallocate mem)                        ; memory arrangements
       (reductions #(conj %1 %2) #{})                  ; arrangements seen at each step
       (map-indexed (fn [idx seen] [idx (count seen)])) ; cardinality of seen against index
       (drop-while (fn [[idx count]] (= idx count)))    ; points where the runtim exceeds cardinality
       (first)                                         ; first time we don't increase seen set
       (second)))                                      ; size of set = ; no reallocations

(def day6-input [4 10 4 1 8 4 9 14 5 1 14 15 0 15 3 5])

(defn day6a-result []
  (run-reallocate day6-input))

(defn day6b-result []
  (let [state (first (drop (day6a-result) (iterate reallocate day6-input )))]
    (run-reallocate state)))

;;; Day 7

(defn parse-line [line]
  (let [re #"^(\w+)\s+\((\d+)\)(?:\s+->\s+(.*))?$"]
    (when-let [matches (re-seq re line)]
      (when-let [[name weight suffix] (rest (first matches))]
        {:name name
         :weight (Integer/parseInt weight)
         :subprograms (when-not (str/blank? suffix) (str/split suffix #",\s+"))}))))

(defn day7-input []
  (->> (str/split-lines (slurp (io/resource "day7.txt")))
       (map parse-line)
       (keep identity)))

(defn day7a [input]
  (let [programs (into #{} (map :name input))
        subprograms (into #{} (apply concat (map :subprograms input)))]
    (set/difference programs subprograms)))

(defn graph [input]
  (into {} (map (fn [x] [(:name x) x]) input)))

(defn day7a-result []
  (first (day7a (day7-input))))

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
          gs (rest (first (re-seq re line)))]
      (instruction (command (reg (nth gs 0))
                            (function (nth gs 1) (arg (nth gs 2))))
                   (guard (reg (nth gs 3))
                          (comparison (nth gs 4))
                          (arg (nth gs 5)))))))

(defn read-program [s]
  (->> (str/split-lines s) (map parse-instruction)))

(def day8-program (read-program (slurp (io/resource "day8.txt"))))

(defn run-program [instructions]
  (reduce #(%2 %1) {} instructions))

(defn day8a-result []
  (apply max (vals (run-program day8-program))))

(defn instrumented [x]
  #(let [state (x %)
         m (reduce max 0 (vals state))]
     (assoc state :max m)))

(defn instrument-program [program]
  (map instrumented program))

(defn day8b-result []
  (:max (run-program (instrument-program day8-program))))

;;; Day 9

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

;; deal with !s as preprocess

(defn clean-cancellations [text]
  (-> text
      (str/replace #"!." "")
      (str/trim-newline)))

(defn run-parse [parser text]
  (if-let [[result residue] (parser text)]
    (if (empty? residue)
      result
      (throw (ex-info "Incomplete parse" {:residue residue})))
    (throw (ex-info "Parse failure" {}))))

;; 9a - parse into nested vector structure

(def garbage (sq (lit \<) (many (satisfying ch #(not= % \>))) (lit \>)))

(def eat-garbage (eat garbage))

(declare group-nesting)

(defn item [text] ((alt eat-garbage group-nesting) text))

(def group-nesting (fmap (sq (lit \{) (many-sep-by (lit \,) item) (lit \}))
                         (fn [[_ xs _]] (vec (remove nil? xs)))))

;;; ...and functions to evaluate the structure

(defn group-count [groups]
  (inc (apply + (map group-count groups))))

(defn group-score
  ([groups]
   (group-score 1 groups))
  ([depth groups]
   (+ depth (apply + (map (partial group-score (inc depth)) groups)))))

(def day9-input (slurp (io/resource "day9.txt")))

(defn day9a-result [] (group-score (run-parse group-nesting (clean-cancellations day9-input))))


;; 9b - alter semantic actions to count garbage instead

(defn counted-garbage [] (fmap garbage (fn [[_ cs _]] (count  cs))))

(declare group-9b)

(defn item-9b [text] ((alt (counted-garbage) (group-9b)) text))

(defn group-9b [] (fmap (sq (lit \{) (many-sep-by (lit \,) item-9b) (lit \}))
                      (fn [[_ xs _]] (apply + (remove nil? xs)))))

(defn day9b-result [] (run-parse (group-9b) (clean-cancellations day9-input)))

;;; Day 10

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

(defn run-10b [n s]
  (let [lengths (lengths-10b s)
        run-once (fn [state] (reduce step state lengths))
        sparse (as-list (:indices (first (drop 64 (iterate run-once (start-state n))))))
        dense (sparse-to-dense sparse)]
    dense))

(def day10b-input "187,254,0,81,169,219,1,190,19,102,255,56,46,32,2,216")

(defn day10b-result []
  (run-10b 256 day10b-input))

;;; Main

(defn -main []
  (println "Day 1 Part One:" (day1-result))
  (println "Day 1 Part Two:" (day1b-result))
  (println "Day 2 Part One:" (day2-result))
  (println "Day 2 Part Two:" (day2b-result))
  (println "Day 3 Part One:" (day3a-result))
  (println "Day 3 Part Two:" (day3b-result))
  (println "Day 4 Part One:" (day4-result))
  (println "Day 4 Part Two:" (day4b-result))
  (println "Day 5 Part One:" (day5a-result))
  (println "Day 5 Part Two:" (day5b-result))
  (println "Day 6 Part One:" (day6a-result))
  (println "Day 6 Part Two:" (day6b-result))
  (println "Day 7 Part One:" (day7a-result))
  (println "Day 7 Part Two:" (day7b-result))
  (println "Day 8 Part One:" (day8a-result))
  (println "Day 8 Part Two:" (day8b-result))
  (println "Day 9 Part One:" (day9a-result))
  (println "Day 9 Part Two:" (day9b-result)))
