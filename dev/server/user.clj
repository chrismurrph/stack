(ns user
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.stacktrace :refer [print-stack-trace]]
            [clojure.math.combinatorics :as combo]))

(defn r []
  (require 'user :reload))

(def width 120)

(defn my-pp
  ([n x]
   (binding [pprint/*print-right-margin* n]
     (-> x pprint/pprint)))
  ([x]
   (my-pp width x)))

(defn probe-on
  ([x]
   (-> x
       my-pp)
   x)
  ([x & msgs]
   (apply println x "<--" msgs)
   x))

(defn x-1 []
  (let [common-prefix (fn [str1 str2]
                        (loop [acc-idx 0]
                          (let [min-len (min (count str1) (count str2))
                                ;_ (println min-len acc-idx)
                                ]
                            (if (= min-len acc-idx)
                              acc-idx
                              (let [val-at-idx-1 (nth str1 acc-idx)
                                    val-at-idx-2 (nth str2 acc-idx)]
                                (if (= val-at-idx-1 val-at-idx-2)
                                  (recur (inc acc-idx))
                                  acc-idx))))))
        space-out (fn [out] (str (count out) " " out))
        ;[str1 str2] (line-seq (java.io.BufferedReader. *in*))
        [str1 str2] ["kitkat" "kit"]
        idx (common-prefix str1 str2)
        in-common (subs str1 0 idx)
        end-1 (subs str1 idx)
        end-2 (subs str2 idx)
        ]
    (println (space-out in-common))
    (println (space-out end-1))
    (println (space-out end-2))))

(defn x-2 []
  (let [some-nums-func (fn [nums-a nums-b]
                         (- nums-a nums-b)
                         )
        str->ints (fn [string]
                    (map #(Integer/parseInt %)
                         (clojure.string/split string #" ")))
        ;input (line-seq (java.io.BufferedReader. *in*))
        input ["5 6 7"
               "3 6 10"]
        [first second] (map str->ints input)
        calc-results (map some-nums-func first second)
        results (reduce (fn [[a b] ele]
                          (cond
                            (pos? ele) [(inc a) b]
                            (neg? ele) [a (inc b)]
                            :default [a b]
                            )
                          )
                        [0 0]
                        calc-results)
        ]
    (let [[a b] results]
      (println (str a " " b))
      )))

(defn x-4 []
  (let [abs (fn [val] (if (neg? val) (* -1 val) val))
        str->ints (fn [string]
                    (map #(Integer/parseInt %)
                         (clojure.string/split string #" ")))
        primary-diag (fn [matrix]
                       (reduce
                         (fn [acc ele]
                           (let [idx (count acc)]
                             (conj acc (nth ele idx))))
                         []
                         matrix))
        ;input (line-seq (java.io.BufferedReader. *in*))
        input ["3"
               "11 2 4"
               "4 5 6"
               "10 8 -12"]
        all-num-lists (map str->ints (drop 1 input))
        primy (reduce + (primary-diag all-num-lists))
        ;_ (println primy)
        secry (reduce + (primary-diag (map reverse all-num-lists)))
        ;_ (println secry)
        res (abs (- primy secry))
        ]
    (println (str res))))

(defn x-5 []
  (let [str->ints (fn [string]
                    (map #(Integer/parseInt %)
                         (clojure.string/split string #" ")))
        ;input (line-seq (java.io.BufferedReader. *in*))
        input ["6"
               "-4 3 -9 0 4 1"]
        line-of-numbers (first (map str->ints (rest input)))
        ;_ (println line-of-numbers)
        results (reduce (fn [{:keys [pos neg zero] :as acc} ele]
                          (cond
                            (pos? ele) (update acc :pos inc)
                            (neg? ele) (update acc :neg inc)
                            :default (update acc :zero inc)
                            )
                          )
                        {:pos 0 :neg 0 :zero 0}
                        line-of-numbers)
        denominator (count line-of-numbers)
        pos (float (/ (:pos results) denominator))
        neg (float (/ (:neg results) denominator))
        zero (float (/ (:zero results) denominator))
        ]
    (println pos)
    (println neg)
    (println zero)))

(defn x-6 []
  (let [
        give-me (fn [n x] (apply str (repeat n x)))
        ;input (line-seq (java.io.BufferedReader. *in*))
        input ["6"]
        num-in (Integer/parseInt (first input))
        ;output [(give-me 5 "#")]
        output (reduce
                 (fn [acc ele]
                   (let [num-spaces (- num-in ele)
                         num-bricks ele
                         ;_ (println num-spaces num-bricks)
                         ]
                     (conj acc (concat (give-me num-spaces " ") (give-me num-bricks "#")))))
                 []
                 (range 1 (inc num-in)))
        ]
    (doseq [x output]
      (println (apply str x)))))

(defn gen-nth [xs ns]
  (for [n ns]
    (nth xs n)))

(defn x-7 []
  (vector
    (gen-nth [:a :b :c :d :e] [2 4])
    (gen-nth [:a :b :c :d :e] (range 3))
    (gen-nth [:a :b :c :d :e] [0])))

(defn pascalls [starting-row]
  (let [gen-next (fn [row-above]
                   (->> (conj (into [0] row-above) 0)
                        (partition 2 1)
                        (map #(apply +' %))))]
    (iterate gen-next starting-row)))

(defn x-8 []
  (take 2 (pascalls [1 2 1])))

(defn trees->tables [m]
  (->> (for [[k1 v1] m
             [k2 v2] v1]
         [[k1 k2] v2])
       (into {})))

(defn x-9 []
  (trees->tables '{a {p 1, q 2}
                   b {m 3, n 4}}))

(defn pairwise-disjoint-1 [set]
  (let [all-in (apply concat set)
        as-set (into #{} all-in)]
    (= (count all-in) (count as-set))))

;;
;; distinct? will tell none are equal, same as above of putting into a set,
;; or going thru each and checking equality against the rest
;;
(defn pairwise-disjoint-2 [set]
  (let [all-in (mapcat identity set)]
    (apply distinct? all-in)))

(defn x-10 []
  (pairwise-disjoint-2 #{#{'(:x :y :z) '(:x :y) '(:z) '()}
                         #{#{:x :y :z} #{:x :y} #{:z} #{}}
                         #{'[:x :y :z] [:x :y] [:z] [] {}}}))

(defn rotate-sequence [n xs]
  (let [num (count xs)
        o (rem n num)
        m (if (neg? o)
            (+ num o)
            o)
        [begin end] [(take m xs) (drop m xs)]]
    (into begin (reverse end))))

(defn x-11 []
  (rotate-sequence -4 '(:a :b :c)))

(defn split-by-type [xs]
  (vals (group-by type xs)))

(defn x-12 []
  (split-by-type [1 :a 2 :b 3 :c]))

(defn count-occurrences [xs]
  (->> xs
       (group-by identity)
       (map (fn [[k v]] [k (count v)]))
       (into {})))

(defn x-13 []
  (count-occurrences [1 1 2 3 2 1 1]))

(defn rem-dups-1 [xs]
  (->> xs
       (group-by identity)
       vals
       (map first)
       sort))

(defn rem-dups-2 [xs]
  (reduce (fn [acc ele]
            (if (contains? (set acc) ele)
              acc
              (concat acc [ele])))
          () xs))

(defn x-14 []
  (rem-dups-2 [:a :a :b :b :c :c]))

(defn my-partition [n xs]
  (loop [res [] xs xs]
    (if (>= (count xs) n)
      (recur (conj res (take n xs)) (drop n xs))
      res)))

(defn x-15 []
  (my-partition 3 (range 8)))

(defn prime? [n]
  (not-any? #(zero? (rem n %)) (range 2 n)))

(defn primes [n]
  (->> (iterate inc 2)
       (filter prime?)
       (take n)))

(defn perfect? [n]
  (->> (filter #(zero? (rem n %)) (range 1 n))
       (reduce +)
       (= n)))

(defn x-24 []
  (perfect? 6))

(fn primes [n]
  (let [prime? (fn [n] (not-any? #(zero? (rem n %)) (range 2 n)))]
    (->> (iterate inc 2)
         (filter prime?)
         (take n))))

(defn x-16 []
  (primes 5))

(defn x-17 []
  (letfn
    [(foo [x y] #(bar (conj x y) y))
     (bar [x y] (if (> (last x) 10)
                  x
                  #(foo x (+ 2 y))))]
    (trampoline foo [] 1)))

(fn insert-between-satisfying [pred v coll]
  (if (seq coll)
    (let [res (->> coll
                   (partition 2 1)
                   (mapcat (juxt
                             (fn [x] (when (apply pred x) v))
                             second))
                   (remove nil?))]
      (cons (first coll) res))
    (list)))

;;
;;
;;
(defn find-type [xs]
  (cond
    (= 1 (:tester (conj xs [:tester 1]))) :map
    (let [added-twice (-> xs
                          (conj :tester)
                          (conj :tester))]
      (= 1 (- (count added-twice) (count xs)))) :set
    (= :test2 (first (-> xs (conj :test1) (conj :test2)))) :list
    :else :vector))

(defn x-18 []
  (find-type []))

#_(defn every-exists? [xs1 xs2]
    (when (= (count xs1) (count xs2))
      (->> (for [letter xs1]
             (clojure.string/index-of xs2 letter))
           (some nil?)
           not)))

(defn anagram-finder-1 [xs]
  (let [every-exists? (fn [xs1 xs2]
                        (when (= (count xs1) (count xs2))
                          (->> (for [letter xs1]
                                 ;(clojure.string/index-of xs2 letter)
                                 (.indexOf xs2 (str letter)))
                               (some #(= -1 %))
                               not)))]
    (loop [[head & tail] xs
           out []]
      (let [new-set (conj (->> tail
                               (filter #(every-exists? head %))
                               set)
                          head)]
        (if (seq tail)
          (recur (remove new-set tail) (conj out new-set))
          (set (filter #(> (count %) 1) out)))))))

(defn anagram-finder-2 [xs]
  (->> xs
       (group-by sort)
       vals
       (map set)
       (filter #(> (count %) 1))
       set))

(defn x-20 []
  (anagram-finder-2 ["meat" "mat" "team" "mate" "eat"] #_["veer" "lake" "item" "kale" "mite" "ever"]))

(defn my-reductions
  ([f initial xs1]
   (->> (iterate (fn [[curr [head & tail]]]
                   [(f curr head) tail])
                 [initial xs1])
        (map first)
        ((fn [xs2]
           (if (vector? xs1)
             (take (inc (count xs1)) xs2)
             xs2)))))
  ([f [head & tail]]
   (my-reductions f head tail)))

(defn x-21 []
  (take 5 (my-reductions + [0 1 2 3 4])))

(defn x-22 []
  (my-reductions conj [1] [2 3 4]))

(defn x-23 []
  (take 5 (my-reductions + (range))))

(defn my-merge-with [f m & rest-m]
  (reduce (fn [m1 m2]
            (let [common-keys (clojure.set/intersection (-> m1 keys set) (-> m2 keys set))
                  m3 (->> common-keys
                          (map (fn [k] [k (f (get m1 k) (get m2 k))]))
                          (into {}))]
              (merge m3 (apply dissoc m1 common-keys) (apply dissoc m2 common-keys))))
          m rest-m))

(defn x-25 []
  (my-merge-with * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5}))

(defn into-camel-case [s]
  (->> (#(str/split % #"-") s)
       ((juxt first #(apply str (map clojure.string/capitalize (next %)))))
       (apply str)))

(defn x-30 []
  (into-camel-case "multi-word-key"))

(defn divisors [n]
  (filter #(zero? (rem n %)) (range 1 (inc n))))

(defn common-divisors [n m]
  (clojure.set/intersection (-> n divisors set) (-> m divisors set)))

(defn coprime? [n m]
  (let [common-divisors (common-divisors n m)]
    (when (seq common-divisors)
      (= 1 (apply max common-divisors)))))

(defn eulers-totient [n]
  (if (= n 1)
    1
    (let [divisors (fn [n] (filter #(zero? (rem n %)) (range 1 (inc n))))
          coprime? (fn [n m]
                     (let [common (clojure.set/intersection (-> n divisors set) (-> m divisors set))]
                       (when (seq common)
                         (= 1 (apply max common)))))]
      (->> (range n)
           (map (partial coprime? n))
           (filter identity)
           count))))

(defn x-31 []
  (eulers-totient 10))

(defn x-32 []
  (common-divisors 5 10))

(defn sum-of-squares [n]
  (->> (str n)
       (map #(Integer/parseInt (str %)))
       (map #(* % %))
       (reduce +)))

(defn happy-number? [n]
  (let [sum-of-squares (fn [n]
                         (->> (str n)
                              (map #(Integer/parseInt (str %)))
                              (map #(* % %))
                              (reduce +)))]
    (->> (iterate sum-of-squares n)
         (take 100)
         (#(= 1 (last %))))))

(defn x-33 []
  (happy-number? 986543210))

;; The trampoline function takes a function f and a variable number of parameters. Trampoline calls f with any
;; parameters that were supplied. If f returns a function, trampoline calls that function with no arguments.
;; This is repeated, until the return value is not a function, and then trampoline returns that non-function value.
;; This is useful for implementing mutually recursive algorithms in a way that won't consume the stack.
(defn my-trampoline [f & params]
  (let [res1 (apply f params)]
    (if (fn? res1)
      (loop [func res1]
        (let [res2 (func)]
          (if (fn? res2)
            (recur res2)
            res2)))
      res1)))

(defn x-34 []
  (letfn [(triple [x] #(sub-two (* 3 x)))
          (sub-two [x] #(stop? (- x 2)))
          (stop? [x] (if (> x 50) x #(triple x)))]
    (my-trampoline triple 2)))

(defn balanced-number? [n]
  (let [sum-f (fn [s]
                (->> s
                     (map #(Integer/parseInt (str %)))
                     (reduce +)))
        s (str n)
        grab-num (int (/ (inc (count s)) 2))
        left (apply str (take grab-num s))
        right (apply str (take grab-num (reverse s)))]
    (= (sum-f left) (sum-f right))))

(defn x-35 []
  (balanced-number? 11112))

(defn longest-increasing-subseq [v]
  (let [structure (->> v
                       (partition 2 1)
                       (map-indexed (fn [idx [a b]]
                                      [idx (= (inc a) b) [a b]]))
                       (partition-by second)
                       (filter #(-> % first second)))
        longest (when (seq structure)
                  (apply max-key count structure))]
    (if longest
      (->> longest
           (mapcat #(nth % 2))
           (take-nth 2)
           (#(concat % [(-> longest last last last)])))
      [])))

(defn x-36 []
  (longest-increasing-subseq [7 6 5 4]))

;; Performance problems
(defn power-set-1 [entry-set]
  (let [list-of-sets-f #(for [s %]
                          (for [e s]
                            (disj s e)))]
    (loop [res []
           in [entry-set]]
      (let [new-sets (mapcat identity (list-of-sets-f in))]
        (if (some seq new-sets)
          (recur (into res new-sets) new-sets)
          (conj (set res) #{} entry-set))))))

;; Again times out.
(defn power-set-2 [entry-set]
  (->> [entry-set]
       (iterate (fn [xs]
                  (mapcat identity (for [s xs]
                                     (for [e s]
                                       (disj s e))))))
       (take-while #(some seq %))
       (mapcat identity)
       (into #{})
       (#(conj % #{} entry-set))))

(defn power-set-3 [entry-set]
  (->> [entry-set]
       (iterate (fn [xs]
                  (mapcat identity
                          (for [s xs]
                            (combo/combinations s (-> s count dec))))))
       (take-while #(some seq %))
       (mapcat identity)
       (map (partial into #{}))
       (into #{})
       (#(conj % #{} entry-set))))

(defn power-set-4 [entry-set]
  (->> [entry-set]
       (iterate (fn [xs]
                  (distinct (mapcat identity (for [s xs]
                                               (for [e s]
                                                 (let [eq? #(= % e)]
                                                   (->> s
                                                        (remove eq?)))))))))
       (take-while #(some seq %))
       (mapcat identity)
       (map (partial into #{}))
       (into #{})
       (#(conj % #{} entry-set))))

(defn power-set [entry-set]
  (->> [entry-set]
       (iterate (fn [xs]
                  (distinct (mapcat identity (for [s xs]
                                               (for [e s]
                                                 (remove (partial = e) s)))))))
       (take-while #(some seq %))
       (mapcat identity)
       (map (partial into #{}))
       (into #{})
       (#(conj % #{} entry-set))))

(def examples [#{1 :a}
               #{}
               #{1 2 3}
               (into #{} (range 10))])

(defn x-37 []
  (let [example (nth examples 2)
        ;res (combo/combinations (into [] example) (-> example count dec))
        res (power-set example)
        ]
    (println res)
    (count res)))

(defn equivalences-2 [f s]
  (->> (group-by f s)
       vals
       (map set)
       set))

(defn x-38 []
  (equivalences-2 #(* % %) #{-2 -1 0 1 2}))

(defn keys-and-vals [xs]
  (->> xs
       (reduce (fn [acc ele]
                 (if (keyword? ele)
                   (conj acc ele [])
                   (update-in acc [(-> acc count dec)] (fn [v] (conj v ele)))))
               [])
       (partition 2)
       (map (fn [[k v]] [k v]))
       (into {})))

(defn x-39 []
  (keys-and-vals [:a 1 2 3 :b :c 4]))

(defn tic-tc-toe [v]
  (let [all-same? #(-> % frequencies first second ((partial = 3)))
        winner? (fn [v]
                  (or
                    (-> (map first v) all-same?)
                    (-> (map second v) all-same?)
                    (= v [[2 0] [1 1] [0 2]])
                    (= v [[0 0] [1 1] [2 2]])))]
    (->> v
         (map-indexed (fn [row-num row] (map-indexed (fn [col-num val] [val [col-num row-num]]) row)))
         (mapcat identity)
         (group-by first)
         (remove (fn [[k v]] (= k :e)))
         (map (fn [[k v]] [k (mapv second v)]))
         (map (fn [[k v]] [k (winner? v)]))
         (some (fn [[k v]] (when v k))))))

(defn x-40 []
  (tic-tc-toe [[:e :x :e]
               [:o :o :o]
               [:x :e :x]]))

(defn decimal->base [n b]
  (let [headings (->> (iterate #(*' % b) b) (take 6) (#(conj % 1)) reverse)]
    (->> (reduce (fn [{:keys [res current :as acc]} heading]
                   (assoc acc :res (conj res (quot current heading)) :current (rem current heading)))
                 {:current n :res []}
                 headings)
         :res
         (drop-while zero?)
         (#(if (seq %) % [0])))))

(defn x-41 []
  (let [n (rand-int 100000)]
    (decimal->base n n)))

(defn oscillating-iterate [v & fs]
  (let [num-fs (count fs)]
    (->> (iterate (fn [[idx res]]
                    [(inc idx) ((nth fs (rem idx num-fs)) res)])
                  [0 v])
         (map second))))

(defn x-42 []
  (->> (oscillating-iterate 3.14 int double)
       (take 3)))

(defn pronounce-number [xs]
  (->> xs
       (partition-by identity)
       (mapcat (fn [xs]
                 [(count xs) (first xs)]))))

(defn pronounce-numbers [xs]
  (->> (iterate #(->> % (partition-by identity) (mapcat (juxt count first)))
                xs)
       (drop 1)))

(defn x-43 []
  (->> (pronounce-numbers [1])
       (take 3)))

(defn answer [f]
  (fn [& args]
    (loop [idx 0
           f f]
      (let [res (f (nth args idx))]
        (if (fn? res)
          (recur (inc idx) res)
          res)))))

(defn x-44 []
  (= 10 (probe-on ((answer (fn [a]
                             (fn [b]
                               (fn [c]
                                 (fn [d]
                                   (+ a b c d))))))
                    1 2 3 4))))

(defn smallest-in-all [xs-of-xs]
  (->> xs-of-xs
       (map set)
       (apply clojure.set/intersection)
       (apply min)))

(defn range-in-all [xs-of-xs]
  (->> xs-of-xs
       (map (juxt #(apply min %) #(apply max %)))))

(defn one-co-temporal-hof [[min-1 max-1]]
  (fn [[min-2 max-2]]
    (and (>= max-1 min-2)
         (<= min-2 max-1))))

;;
;; If at an index we get a match, then mark that as false (no need to go further)
;; at that index.
;;
(defn all-co-temporal [grab-size xs-of-xs most-advanced]
  (let [most-advanced-range ((juxt #(apply min %) #(apply max %)) most-advanced)
        one-co-temporal-f (one-co-temporal-hof most-advanced-range)]
    (loop [states (map-indexed (fn [idx v]
                                 (into v [idx])) (repeat (count xs-of-xs) [nil true]))
           idx 0]
      (if (every? (comp not second) states)
        states
        (let [bs (->> xs-of-xs
                      (map (partial drop (* idx grab-size)))
                      (map (partial take grab-size))
                      (map (juxt #(apply min %) #(apply max %)))
                      (map (complement one-co-temporal-f)))
              new-states (map (fn [b] [idx b]) bs)]
          (recur new-states (inc idx)))))))

;;
;; Start off finding the most advanced batch in a grab of 10 from each.
;; Then have a fn that rets true if need to go further to get to the advanced batch.
;; Have go loop retains an index for them all. [idx true/false], where true means need to
;; read the next index. So at beginning at least one will be [0 false], and the rest [0 true].
;; Stop looping when all are false.
;; Answer will come from calling smallest-in-all when all are [0 false]
;;
(defn x-45 []
  (let [in [[1 2 3 4 5 6 7] [0.5 3/2 4 5 19] [-4 0 1 2 3 4]]
        grab-size 2
        first-batches (map (partial take grab-size) in)
        most-advanced (->> first-batches
                           probe-on
                           (reduce (fn [acc ele]
                                     (if (> (last ele) (last acc))
                                       ele
                                       acc))))]
    most-advanced))

(defn smallest-in-all-1 [& lazy-seqs]
  (let [curr-val-f (fn [lazy-indexes which-lazy]
                     (let [curr-idx (nth lazy-indexes which-lazy)
                           curr-lazy (nth lazy-seqs which-lazy)]
                       (nth curr-lazy curr-idx)))
        prior-index-f (fn [n curr-lazy]
                        (if (pos? curr-lazy)
                          (- curr-lazy n)
                          (- (count lazy-seqs) n)))
        prior-val-f (fn [n lazy-indexes which-lazy]
                      (try (nth (nth lazy-seqs (prior-index-f n which-lazy)) (nth lazy-indexes (prior-index-f n which-lazy)))
                           (catch Exception _ nil)))
        prev-val-f (partial prior-val-f 1)
        ;; If are lower than current value in last sequence then move index forward so are just greater than it.
        move-forward-hof (fn [which-lazy lazy-indexes]
                           (fn [n]
                             (let [prev-val (prev-val-f lazy-indexes which-lazy)
                                   this-lazy (nth lazy-seqs which-lazy)]
                               (loop [num-incs 0
                                      idx n]
                                 (println "idx has become" idx "on" this-lazy)
                                 (if (<= (nth this-lazy idx) prev-val)
                                   (if (= (inc idx) (count this-lazy))
                                     (do
                                       (println "reached end")
                                       num-incs)
                                     (if (= (nth this-lazy idx) prev-val)
                                       (do
                                         (println "equal values")
                                         num-incs)
                                       (recur (inc num-incs) (inc idx))))
                                   (do
                                     (println "prev value is less")
                                     0))))))]
    (loop [which-lazy 0
           lazy-indexes (vec (take (count lazy-seqs) (repeat 0)))
           times 0]
      (when (< times 15)
        (let [how-many (count lazy-seqs)
              curr-val (curr-val-f lazy-indexes which-lazy)
              last-val (prev-val-f lazy-indexes which-lazy)
              move-forward-f (move-forward-hof which-lazy lazy-indexes)]
          (println "last" last-val "curr" curr-val)
          (if (= last-val curr-val)
            (if (let [buffer (mapv #(nth %2 (nth lazy-indexes %1)) (range how-many) lazy-seqs)]
                  (println "buffer" buffer)
                  (every? (partial = curr-val) buffer))
              curr-val
              (recur (rem (inc which-lazy) how-many) lazy-indexes (inc times)))
            (recur (rem (inc which-lazy) how-many) (vec (update-in lazy-indexes [which-lazy] move-forward-f)) (inc times))))))))

;;
;; Have buffer holds one spot for each
;; Read until goes equal or over last one
;; If equal see if the whole buffer is =, and we are done
;; If not store one over equal.
;; Loop has index and buffer and access to the infinite-s
;;
(defn x-50 []
  (smallest-in-all-1 [1 2 3 4 5 6 7] [0.5 3/2 4 19]))

(defn smallest-in-all-2 [& xss]
  (let [firsts (map first xss)]
    (if (every? (partial = (first firsts)) firsts)
      (first firsts)
      (recur (for [xs xss]
               (drop-while #(< % (apply max firsts)) xs))))))

;;
;; Look at first spot and if all equal that's the answer
;; Otherwise find greatest and drop while < it.
;; So here 2 is greatest:
;; [1 2 3 4 5 6 7] [0.5 3/2 4 19] [2 3.5 4 7]
;; Becomes:
;; [2 3 4 5 6 7] [4 19] [3.5 4 7]
;; Now 4 is greatest so becomes:
;; [4 5 6 7] [4 19] [4 7]
;; Now all are equal so that's the answer
;;
;;
;;
(defn x-51 []
  (smallest-in-all-2 [1 2 3 4 5 6 7] [0.5 3/2 4 19] [3.5 4 7]))

(defn partially-flatten-1 [xss]
  (for [xs xss]
    (loop [xs xs]
      (if (and (sequential? xs) (sequential? (first xs)))
        (recur (apply concat xs))
        xs))))

(defn partially-flatten-2 [xss]
  (let [res (atom [])]
    (clojure.walk/postwalk (fn [x]
                             (when (and (sequential? x) (sequential? (first x)))
                               (swap! res concat x))) xss)
    @res))

;;
;; Must involve selective recursion on many. So perhaps recursion in the mapping function.
;;
(defn partially-flatten-3 [xs]
  (let [sequent? (every-pred (complement string?) sequential?)
        separate (juxt filter remove)
        go-deeper? (fn [x] (let [res (and (sequent? x) (some sequent? x))]
                             ;(println "[go-deeper? <" x "><" res ">]")
                             res))
        desired-node? (fn [x]
                        (let [res (and (sequent? x) (every? #(not (sequent? %)) x))]
                          ;(println "[desired-node? <" x "><" res ">]")
                          res))]
    (->> (loop [xs xs
                res []
                depth 0]
           (cond
             (desired-node? xs)
             (into [[depth]] (into [xs] res))

             (go-deeper? xs)
             (if (every? desired-node? xs)
               (into [[depth]] (into res xs))
               (let [[deepers rets] (separate (some-fn desired-node? go-deeper?) xs)]
                 ;(println "rets" rets)
                 ;(println "type" (type res))
                 (recur
                   (apply concat deepers)
                   (into [[depth]] (into res [rets]))
                   (inc depth))))
             ))
         (filter seq)
         )))

;;
;; Must involve selective recursion on many. So perhaps recursion in the mapping function.
;;
(defn partially-flatten-3 [xs]
  (let [sequent? (every-pred (complement string?) sequential?)
        separate (juxt filter remove)
        go-deeper? #(and (sequent? %) (some sequent? %))
        desired-node? #(and (sequent? %) (every? (complement sequent?) %))]
    (->> (loop [xs xs res []]
           (cond
             (desired-node? xs) (into [xs] res)
             (go-deeper? xs) (if (every? desired-node? xs)
                               (into xs res)
                               (let [[deepers rets] (separate (some-fn desired-node? go-deeper?) xs)]
                                 (recur
                                   (apply concat deepers)
                                   (into [rets] res))))))
         (filter seq))))

(defn partially-flatten-4 [xs]
  (if (every? sequential? xs)
    (mapcat partially-flatten-4 xs)
    [xs]))

;; Want
;; '((1 2)(3 4)(5 6))
(defn x-52 []
  [(partially-flatten-3 '((1 2) ((3 4) ((((5 6)))))))
   (partially-flatten-3 [[1 2] [[3 4] [[[[5 6]]]]]])
   (partially-flatten-3 [["Do"] ["Nothing"]])
   (partially-flatten-3 [[[[:a :b]]] [[:c :d]] [:e :f]])])

;; Want
;; '((1 2)(3 4)(5 6))
(defn x-55 []
  [(partially-flatten-4 '((1 2) ((3 4) ((((5 6)))))))
   (partially-flatten-4 [[1 2] [[3 4] [[[[5 6]]]]]])
   (partially-flatten-4 [["Do"] ["Nothing"]])
   (partially-flatten-4 [[[[:a :b]]] [[:c :d]] [:e :f]])])

(defn global-take-while [n p xs]
  (loop [curr xs
         res '()
         matched-count 0]
    (let [satisfies (take-while p curr)
          new-matched-count (if (seq satisfies)
                              (+ matched-count (count satisfies))
                              matched-count)]
      (if (< new-matched-count n)
        (recur (drop (max 1 (count satisfies)) curr)
               (concat res satisfies)
               new-matched-count)
        (take-while #(not= % (last satisfies)) xs)))))

(defn x-53 []
  (global-take-while 4 #(= 2 (mod % 3))
                     [2 3 5 7 11 13 17 19 23])
  #_(global-take-while 3 #(some #{\i} %) ["this" "is" "a" "sentence" "i" "wrote"])
  )

(defn roman->decimal [s]
  (->> s
       (partition-by identity)
       (mapv (juxt #(-> % first {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000})
                   count))
       (#(into % [[]]))
       (partition 2 1)
       (reduce (fn [acc [[decimal-value size] [following-value _]]]
                 ((if (> (or following-value 0) decimal-value) - +)
                   acc
                   (* decimal-value size)))
               0)))

(defn x-54 []
  (map roman->decimal ["XIV"
                       "DCCCXXVII"
                       "MMMCMXCIX"
                       "XLVIII"]))

;; Given one, returns two
(defn extender-hof [triangle]
  (fn [row-idx]
    (let [this-row-count (inc row-idx) #_(count (nth triangle row-idx))
          next-row (nth triangle (inc row-idx))]
      (map (fn [idx]
             [(nth next-row idx) (nth next-row (inc idx))])
           (range this-row-count)))))

;; Given the number of rows in a triangle s/be able to work out the possible routes
;;

(defn x-56 []
  (let [tri [[1]
             [2 4]
             [5 1 4]
             [2 3 4 5]]]
    ((extender-hof tri) 2)))

;;
;; co-ordinates are [col row] with [0 0] at the top.
;; Here [[0 0]] will return [[[0 0] [0 1]][[0 0] [1 1]]]
;; We take the last co-ordinate of the path and use it to create two new paths
;;
(defn one->two-extender [path]
  (let [[end-col end-row] (last path)
        lower-left-ending [end-col (inc end-row)]
        lower-right-ending [(inc end-col) (inc end-row)]]
    [(into path [lower-left-ending]) (into path [lower-right-ending])]))

(defn co-ord->value-hof [tri]
  (fn [[col row]]
    (get-in tri [row col])))

(defn x-57 []
  (let [co-ord->value (co-ord->value-hof [[1]
                                          [2 4]
                                          [5 1 4]
                                          [2 3 4 5]])]
    (let [res (first (one->two-extender [[0 0] [0 1]]))]
      (map co-ord->value res))))

(defn iteratee [paths]
  (mapcat one->two-extender paths))

(defn min-path [tri]
  (let [tri (into [] tri)
        co-ord->value ((fn [tri]
                         (fn [[col row]]
                           (get-in tri [row col]))) tri)
        one->two-extender (fn [path]
                            (let [[end-col end-row] (last path)
                                  lower-left-ending [end-col (inc end-row)]
                                  lower-right-ending [(inc end-col) (inc end-row)]]
                              [(into path [lower-left-ending]) (into path [lower-right-ending])]))
        iteratee (fn [paths] (mapcat one->two-extender paths))]
    (->> (iterate iteratee [[[0 0]]])
         (take (count tri))
         last
         (map (fn [co-ords] (map co-ord->value co-ords)))
         (map (partial apply +))
         (apply min))))

(defn x-58 []
  (min-path '([1]
               [2 4]
               [5 1 4]
               [2 3 4 5])))

(defn prime? [n]
  (not-any? #(zero? (rem n %)) (range 2 n)))

(defn primes [n]
  (->> (iterate inc 2)
       (filter prime?)
       (take n)))

(defn prime-sandwich [n]
  (let [[a b c] (->> (iterate inc 2)
                     (filter (fn [n] (not-any? #(zero? (rem n %)) (range 2 n))))
                     (partition 3 1)
                     (drop-while (fn [[_ b _]] (< b n)))
                     first)]
    (and (= b n) (= (/ (+ a c) 2) b))))

(defn x-59 []
  (prime-sandwich 4))

(defn combinations [sz population]
  (->> (cond
         (= sz 0) '(())
         (empty? population) '()
         :else (concat (mapv #(cons (first population) %) (combinations (dec sz) (rest population)))
                       (combinations sz (rest population))))
       (map set)
       (into #{})))

(defn x-60 []
  (->> (combinations 2 #{0 1 2})))

(defn make-interval [{:keys [rest intervals]}]
  (let [interval (->> (cons (-> rest first dec) rest)
                      (partition 2 1)
                      (take-while (fn [[a b]]
                                    (= 1 (- b a))))
                      (map second))
        ending (last interval)]
    {:intervals (into intervals [[(first interval) ending]])
     :rest      (->> rest
                     (drop-while #(>= ending %)))}))

(defn intervals [xs]
  (let [make-interval (fn [{:keys [rest intervals]}]
                        (let [interval (->> (cons (-> rest first dec) rest)
                                            (partition 2 1)
                                            (take-while (fn [[a b]]
                                                          (= 1 (- b a))))
                                            (map second))
                              ending (last interval)]
                          {:intervals (into intervals [[(first interval) ending]])
                           :rest      (->> rest
                                           (drop-while #(>= ending %)))}))]
    (if (seq xs)
      (->> (iterate make-interval {:rest      (sort (set xs))
                                   :intervals []})
           (drop 1)
           (drop-while #(-> % :rest seq))
           first
           :intervals)
      [])))

(defn x-61 []
  #_(make-interval {:rest      (sort [10 9 8 1 2 3])
                    :intervals []})
  (intervals []))

(def cmds {'+ +
           '/ /
           '* *
           '- -})

(defn uce-1 [f]
  (let [op (first f)
        operands (rest f)]
    (fn [m]
      (apply (cmds op) (map #(if (number? %) % (m %)) operands)))))

(defn partially-flatten-4 [xs]
  (if (every? sequential? xs)
    (mapcat partially-flatten-4 xs)
    [xs]))

(defn uce-2 [m]
  (fn calc [f]
    (let [op (first f)
          operands (map #(if (sequential? %)
                           (calc %)
                           %) (rest f))]
      (apply (cmds op) (map #(if (number? %) % (m %)) operands)))))

(defn uce-3 [f]
  (let [cmds {'+ +
              '/ /
              '* *
              '- -}]
    (fn [m]
      (let [op (first f)
            operands (map #(if (sequential? %)
                             ((uce-3 %) m)
                             %)
                          (rest f))]
        (apply (cmds op) (map #(if (number? %) % (m %)) operands))))))

(defn x-62 []
  ((uce-2 '{b 8 a 16})
    '(/ a b)))

(defn x-63 []
  ((uce-2 '{a 1 b 8})
    '(* (+ 2 a)
        (- 10 b))))

(defn x-64 []
  ((uce-3 '(* (+ 2 a)
              (- 10 b)))
    '{a 1 b 8}))

(defn roman-denominations [n [denomination next-denomination]]
  (let [res (when (or (nil? next-denomination) (< n next-denomination))
              (let [res (quot n denomination)]
                (if (< res 4)
                  [[denomination res]]
                  [[denomination (quot (- next-denomination (* res denomination)) denomination)] [next-denomination 1]])))]
    ;(println n denomination next-denomination res)
    res))

(defn x-65 []
  (roman-denominations 4 [1 5]))

(def roman-numerals {1 \I 5 \V 10 \X 50 \L 100 \C 500 \D 1000 \M})

;;
;; No good b/c not catching say 9 being IX.
;;
(defn roman-numeral-1 [n]
  (let [roman-numerals {1    \I
                        5    \V
                        10   \X
                        50   \L
                        100  \C
                        500  \D
                        1000 \M}
        roman-denominations (fn [n [denomination next-denomination]]
                              (when (or (nil? next-denomination) (< n next-denomination))
                                (let [res (quot n denomination)]
                                  (if (or (< res 4) #_(not= (rem denomination 10) 0))
                                    [[denomination res]]
                                    [[denomination (quot (- next-denomination (* res denomination)) denomination)] [next-denomination 1]]))))]
    (->> (loop [[head & tail] [[1000 nil] [500 1000] [100 500] [50 100] [10 50] [5 10] [1 5]]
                number-left n
                result []]
           (let [res (roman-denominations number-left head)
                 res-count (if (= 1 (count res))
                             (->> res first (apply *))
                             (let [[[a b] [c d]] res]
                               (- (* c d) (* a b))))
                 new-left (when res (- number-left res-count))]
             (cond
               (nil? res) (recur tail number-left result)
               (nil? tail) (into result res)
               :else (recur tail new-left (into result res)))))
         (filter #(-> % second pos?))
         (map (fn [[a b]]
                [(roman-numerals a) b]))
         (map (fn [[a b]]
                (apply str (repeat b a))))
         (apply str))))

;; CMXCIX
(defn x-66 []
  (roman-numeral-1 999))

;; 9 s/be IX, getting VIV
(defn x-67 []
  (roman-numeral-1 9))

(defn x-68 []
  (roman-numeral-1 3549))

(def ordered-bases [[1000 "M"]
                    [900 "CM"]
                    [500 "D"]
                    [400 "CD"]
                    [100 "C"]
                    [90 "XC"]
                    [50 "L"]
                    [40 "XL"]
                    [10 "X"]
                    [9 "IX"]
                    [5 "V"]
                    [4 "IV"]
                    [1 "I"]])

;;
;;
;;
(defn roman-numeral-2 [n]
  (->> (loop [[[denomination symbol] & tail] [[1000 "M"] [900 "CM"] [500 "D"] [400 "CD"] [100 "C"] [90 "XC"] [50 "L"] [40 "XL"] [10 "X"] [9 "IX"] [5 "V"] [4 "IV"] [1 "I"]]
              number-left n
              result []]
         (let [times (quot number-left denomination)
               accum (into result [[denomination times symbol]])]
           (if tail
             (recur tail
                    (- number-left (* denomination times))
                    accum)
             accum)))
       (remove #(-> % second zero?))
       (map (fn [[_ n sym]] (apply str (repeat n sym))))
       (apply str)))

(defn x-69 []
  (roman-numeral-2 3549))

(defn balanced-brackets-1 [s]
  (let [closed? (fn [[depth position]]
                  (zero? depth))
        open? (fn [[depth position ch]]
                (pos? depth))
        closing-while-another-open? (fn [head currents]
                                      (let [open-one (some #(when open? %) currents)]
                                        (println currents)
                                        (and (#{\) \} \]} head))))]
    (loop [idx 0
           [head & tail] s
           [paren-depth paren-position :as paren] [0 nil]
           [braces-depth braces-position :as braces] [0 nil]
           [square-bracket-depth square-bracket-position :as square-bracket] [0 nil]]
      (let [[new-paren new-braces new-square-bracket :as news]
            (condp = head
              \( [[(inc paren-depth) idx] braces square-bracket]
              \{ [paren [(inc braces-depth) idx] square-bracket]
              \[ [paren braces [(inc square-bracket-depth) idx]]
              \) [[(dec paren-depth) idx] braces square-bracket]
              \} [paren [(dec braces-depth) idx] square-bracket]
              \] [paren braces [(dec square-bracket-depth) idx]]
              [paren braces square-bracket])]
        (if tail
          (when-not (closing-while-another-open? head (mapv #(into %1 [%2]) [paren braces square-bracket] [\) \} \]]))
            (recur (inc idx) tail new-paren new-braces new-square-bracket))
          (every? closed? [new-paren new-braces new-square-bracket]))))))

(defn balanced-brackets-2 [s]
  (loop [[head & tail] s
         paren-depth 0
         braces-depth 0
         square-bracket-depth 0]
    (let [[new-paren new-braces new-square-bracket]
          (condp = head
            \( [(inc paren-depth) braces-depth square-bracket-depth]
            \{ [paren-depth (inc braces-depth) square-bracket-depth]
            \[ [paren-depth braces-depth (inc square-bracket-depth)]
            \) [(dec paren-depth) braces-depth square-bracket-depth]
            \} [paren-depth (dec braces-depth) square-bracket-depth]
            \] [paren-depth braces-depth (dec square-bracket-depth)]
            [paren-depth braces-depth square-bracket-depth])]
      (if tail
        (recur tail new-paren new-braces new-square-bracket)
        (every? zero? [new-paren new-braces new-square-bracket])))))

(defn balanced-brackets-3 [s]
  (let [not-noise #{\( \) \} \{ \] \[}
        openness #(cond (#{\( \{ \[} %) :open (#{\) \} \]} %) :close)
        typeof #(cond (#{\( \)} %) :paren (#{\} \{} %) :braces (#{\] \[} %) :square-bracket)
        balanced-f? #(loop [[head & tail] %
                            depths {:paren 0 :braces 0 :square-bracket 0}]
                       (let [new-depths (if (not-noise head)
                                          (update-in depths
                                                     [(typeof head)]
                                                     (condp = (openness head) :open inc :close dec))
                                          depths)]
                         (if tail (recur tail new-depths) (every? zero? (vals new-depths)))))]
    (let [bad-nesting? (->> s
                            (filter not-noise)
                            (partition 2 1)
                            (filter (fn [xs] (and (= [:open :close] (map openness xs))
                                                  (apply not= (map typeof xs)))))
                            seq)]
      (when (not bad-nesting?) (balanced-f? s)))))

(defn x-70 []
  (balanced-brackets-3 "This string has no brackets." #_"())" #_"[ { ] } "))

(defn sequs-horribilis-1 [n xs accum-in]
  (assert (number? accum-in))
  (let [local-recur (fn [{:keys [head depth] :as node}]
                      (assert (number? depth))
                      (if (sequential? head)
                        (do
                          (println "head" head "accum" depth "first of head" (first head))
                          (sequs-horribilis-1 n head depth))
                        node))]
    (loop [[head & tail] xs
           res []
           accum accum-in]
      (assert (number? accum))
      (let [node {:head head :depth (if (number? head)
                                      (+ head accum)
                                      accum)}]
        (if tail
          (recur tail (conj res (local-recur node)) (if (number? head)
                                                      (+ head accum)
                                                      accum))
          (conj res (local-recur node)))))))

(defn sequs-horribilis-2 [n xs depth-in]
  (let [local-recur (fn [{:keys [head depth] :as node}]
                      (if (sequential? head)
                        (sequs-horribilis-2 n head (inc depth))
                        node))]
    (loop [[head & tail] xs
           res []
           depth depth-in]
      (let [node {:head head :depth depth}]
        (if tail
          (recur tail (conj res (local-recur node)) depth)
          (conj res (local-recur node)))))))

(defn sequs-horribilis-3 [n-in xs-in]
  (letfn [(sequs ([n xs depth-in]
                  (let [local-recur (fn [{:keys [head depth] :as node}]
                                      (if (sequential? head)
                                        (sequs n head (inc depth))
                                        node))]
                    (loop [[head & tail] xs
                           res []
                           depth depth-in]
                      (let [node {:head head :depth depth}]
                        (if tail
                          (recur tail (conj res (local-recur node)) depth)
                          (conj res (local-recur node))))))))]
    (->> (sequs n-in xs-in 0)
         flatten
         (cons {:depth 0})
         (partition 2 1))))

(defn sequs-horribilis-4 [n-in xs-in]
  (letfn [(maybe-recur ([h d n]
                        (if (sequential? h)
                          (sequs n h (inc d))
                          {:num h :depth d})))
          (sequs ([n xs depth-in]
                  (let []
                    (loop [[h & t] xs
                           res []
                           depth depth-in]
                      (if t
                        (recur t (conj res (maybe-recur h depth n)) depth)
                        (conj res (maybe-recur h depth n)))))))
          (this-fn ([n xs]
                     ;(println xs)
                    (loop [[{:keys [num depth-diff] :as h} & t] xs
                           accum 0
                           res []]
                      ;(println "num" num)
                      (assert num xs)
                      (let [new-accum (+ num accum)
                            new-res (if (and depth-diff (pos? depth-diff))
                                      (conj res (this-fn 0 [(assoc h :depth-diff 0)]))
                                      (conj res h))]
                        (if (<= new-accum n)
                          (recur t
                                 new-accum
                                 new-res)
                          new-res)))))]
    (->> (sequs n-in xs-in 0)
         flatten
         (cons {:depth 0})
         (partition 2 1)
         (map (fn [[{depth-a :depth} {depth-b :depth num :num}]]
                {:num num :depth-diff (- depth-b depth-a)}))
         (this-fn n-in))))

;; Next loop/recur with in, out, accum so can short circuit when have reached the right number
;; Then partition by depth and reduce thru that
(defn x-71 []
  (->> (sequs-horribilis-4 10 [1 2 [3 [4 5] 6] 7])))

(defn sequs-horribilis-5 [n-in xs-in]
  (letfn [(maybe-recur ([h d n a]
                        (if (sequential? h)
                          (sequs n h (inc d) (+ a (if (sequential? h) 0 h)))
                          {:num h :depth d :acc (+ a (if (sequential? h) 0 h))})))
          (sequs ([n xs depth-in accum]
                  (loop [[h & t] xs
                         res []
                         depth depth-in
                         acc accum]
                    (assert (number? acc))
                    (if t
                      (recur t
                             (conj res
                                   (maybe-recur h depth n acc))
                             depth
                             (+ acc (if (sequential? h) 0 h)))
                      (conj res (maybe-recur h depth n acc))))))]
    (->> (sequs n-in xs-in 0 0))))

(defn x-72 []
  (->> (sequs-horribilis-5 10 [1 2 [3 [4 5] 6] 7])))

(defn x-73 []
  (->> (flatten [1 2 [3 [4 5] 6] 7])
       (reductions +)
       (take-while #(<= % 10))
       count))

(defn sequs-horribilis-6 [n-in xs-in]
  (let [out-count (->> (flatten xs-in)
                       (reductions +)
                       (take-while #(< % n-in))
                       count)]
    (letfn [(maybe-recur ([h n idx]
                          (if (sequential? h)
                            (sequs n h idx)
                            (if (<= idx out-count)
                              h
                              nil))))
            (sequs ([n xs idx]
                    (loop [[h & t] xs
                           res []
                           idx idx]
                      (let [maybe-res (maybe-recur h n idx)]
                        (if (nil? maybe-res)
                          res
                          (if (nil? t)
                            (conj res maybe-res)
                            (recur t
                                   (conj res
                                         maybe-res)
                                   (inc idx))))))))]
      (->> (sequs n-in xs-in 0)))))

(defn x-74 []
  (->> (sequs-horribilis-6 10 [1 2 [3 [4 5] 6] 7])))

(defn sequs-horribilis-7 [upper xs]
  (let [cost (fn [ele]
               (if (sequential? ele)
                 (apply + (flatten ele))
                 ele))
        fit (fn [n c]
              (if (sequential? n)
                (sequs-horribilis-7 c n)
                (do
                  (println "At" n "c is" c)
                  (when (<= c n) c))))
        allowances (reductions #(- %1 (cost %2)) upper xs)]
    ;(println allowances)
    (->> (map fit xs allowances)
         (take-while #(or (number? %) (seq %)))
         )))

(defn prune-1 [n xs]
  (let [cost (fn [node] (if (sequential? node) (apply + (flatten node)) node))
        fit (fn [n node] (if (sequential? node) (prune-1 n node) (when (<= node n) node)))
        allowances (reductions #(- % (cost %2)) n xs)
        pruned (map fit allowances xs)]
    (println "allowances" allowances)
    (take-while #(or (number? %) (seq %)) pruned)))

(defn x-75 []
  (->> (sequs-horribilis-7 10 [1 2 [3 [4 5] 6] 7])))

(defn x-76 []
  (->> '(1 2 (3 (4 nil) nil) nil)
       (take-while #(or (number? %) (seq %)))))

(defn x-77 []
  (seq [4 nil 5]))

;; (1 2 (3 (4 (5 (6 (7))))))
(defn x-78 []
  (prune-1 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11]))

;;
;; Reduction for the cost of each one.
;; Output nil in map if not less than or equal to n
;; Thus trim by take-while not-nil (alternatively number?)
(defn prune-2 [n xs]
  (let [allowances (reductions #(- %1 %2) n xs)]
    (->> (map (fn [a x]
                (println "cf" a x)
                (when (>= a 0) x)) allowances xs)
         (take-while number?))))

(defn x-79 []
  (prune-2 5 [1 2 3 4 5]))
