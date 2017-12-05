(ns adventofcode.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

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

(defn normalise-word [word]
  (apply hash-set (map (fn [[k v]] [k (count v)]) (group-by identity word))))

(defn valid-passphrase-part2? [words]
  (let [normalised (into #{} (map normalise-word words))]
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
  (println "Day 5 Part Two:" (day5b-result)))
