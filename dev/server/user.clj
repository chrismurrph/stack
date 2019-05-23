(ns user
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.pprint :as pprint]
            [clojure.stacktrace :refer [print-stack-trace]]
            [clojure.math.combinatorics :as combo]
            [stopwatch :as sw]))

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

(def probe-off identity)

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

(defn prune-1 [n xs]
  (let [cost (fn [node] (if (sequential? node) (apply + (flatten node)) node))
        fit (fn [n node] (if (sequential? node) (prune-1 n node) (when (<= node n) node)))
        allowances (reductions #(- % (cost %2)) n xs)
        pruned (map fit allowances xs)]
    (println "allowances" allowances)
    (take-while #(or (number? %) (seq %)) pruned)))

(defn x-71 []
  (->> '(1 2 (3 (4 nil) nil) nil)
       (take-while #(or (number? %) (seq %)))))

(defn x-72 []
  (seq [4 nil 5]))

;; (1 2 (3 (4 (5 (6 (7))))))
(defn x-73 []
  (prune-1 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11]))

;;
;; We want the ones to
;;
(defn prune-2 [n xs]
  (let [node-cost (fn [x] (if (sequential? x) (->> x flatten (apply +)) x))
        allowances (reductions #(- %1 (node-cost %2)) n xs)]
    (println "allowances" allowances)
    (println "from      " xs)
    (->> (map (fn [a x]
                (if (sequential? x)
                  (prune-2 a x)
                  (when (>= a x) x)))
              allowances xs)
         (take-while (some-fn number? sequential?))
         vec)))

(defn x-74 []
  (prune-2 5 [1 2 3 4 5]))
(defn x-79 []
  (prune-2 10 [1 2 [3 [4 5] 6] 7]))

(defn divides [n step]
  (->> (range 0 n step)
       (apply +)))

(defn big-divide-1 [n s1 s2]
  (let [summed-divides (fn [n step] (range 0 n step))]
    (apply + (into #{} (concat (summed-divides n s1) (summed-divides n s2))))))

(defn big-divide-2 [n s1 s2]
  (let [iteree (fn [x]
                 (cond
                   (zero? (/ x s1)) x
                   (zero? (/ x s2)) x
                   :else nil
                   ))]
    (->> (iterate iteree 0)
         (keep identity)
         (take-while #(< % n)))))

(defn big-divide-working [n s1 s2]
  (let [divides (fn [n step] (range 0 n step))]
    (apply + (distinct (concat (divides n s1) (divides n s2))))
    ))

(defn big-divide-3 [n s1 s2]
  (let [iteree (fn [[a b _ res]]
                 (let [a-added (+ a s1)
                       b-added (+ b s2)
                       lowest (min a-added b-added)]
                   (if (= a-added b-added)
                     [a-added b-added a-added (conj res a-added)]
                     (if (= lowest a-added)
                       [a-added b a-added (conj res a-added)]
                       [a b-added b-added (conj res b-added)]))))]
    (->> (iterate iteree [0 0 0 []])
         (drop-while (fn [[a b last-added res]]
                       (< last-added n)))
         first
         last
         butlast
         (apply +)
         )))

;; Try adding as we go along
(defn big-divide-4 [n s1 s2]
  (let [iteree (fn [[a b _ res]]
                 (let [a-added (+ a s1)
                       b-added (+ b s2)
                       lowest (min a-added b-added)]
                   (if (= a-added b-added)
                     [a-added b-added a-added (+ res a-added)]
                     (if (= lowest a-added)
                       [a-added b a-added (+ res a-added)]
                       [a b-added b-added (+ res b-added)]))))]
    (->> (iterate iteree [0 0 0 0])
         (drop-while (fn [[a b last-added res]]
                       (< last-added n)))
         first
         last
         (#(- % n))
         )))

;; Adding together the result of adding is really multiplying
(defn big-divide-5 [n s1 s2]
  (let [times-s1 (inc (quot (dec n) s1))
        times-s2 (inc (quot (dec n) s2))
        s1-seq (->> (iterate (partial + s1) 0)
                    (take times-s1))
        s2-seq (->> (iterate (partial + s2) 0)
                    (take times-s2))]
    ;[[times-s1 times-s2] [s1-seq s2-seq]]
    (+ (apply + (distinct (concat s1-seq s2-seq))))
    ))

(defn ar-sum [step n]
  (let [num-steps (quot n step)
        last-num (* num-steps step)
        mid-way (/ last-num 2)
        sum (* mid-way (inc num-steps))]
    sum))

;; We can calculate the common ones and remove them from one of the sequences
;; Which is equivalent to minus-ing them once
(defn big-divide-6 [n s1 s2]
  (let [together (* s1 s2)
        times-together (inc (quot (dec n) together))
        times-s1 (inc (quot (dec n) s1))
        times-s2 (quot (dec n) s2)
        s1-seq (->> (iterate (partial + s1) 0)
                    (take times-s1))
        s1-seq (ar-sum s1 times-s1)
        s2-seq (->> (iterate (partial + s2) 0)
                    (take times-s2))]
    (- (+ s1-seq (apply + s2-seq))
       (apply + (->> (iterate (partial + together) 0)
                     (take times-together))))))

(defn ar-sum [step n]
  (let [num-steps (quot (dec n) step)
        last-num (* num-steps step)
        mid-way (/ last-num 2)]
    (* mid-way (inc num-steps))))

(defn x-80 []
  (big-divide-6 1000 3 5))

;; Test why this formula doesn't work
(defn x-81 []
  (let [num 3
        n 10
        times-s1 (inc (quot (dec n) num))
        _ (println "times" times-s1)
        s1-seq (->> (iterate (partial + num) 0)
                    (take times-s1))
        ]
    [s1-seq
     (apply + s1-seq)
     (ar-sum num n)]))

(defn x-82 []
  (let [n 13
        step 3
        num-steps (quot n step)
        last-num (* num-steps step)
        mid-way (/ last-num 2)
        sum (* mid-way (inc num-steps))]
    sum))

(defn big-divide-7 [n s1 s2]
  (let [sum-f (fn [step n]
                (let [num-steps (quot (dec n) step)
                      last-num (* num-steps step)
                      mid-way (/ last-num 2)]
                  (* mid-way (inc num-steps))))
        s1-sum (sum-f s1 n)
        s2-sum (sum-f s2 n)
        together-sum (sum-f (* s1 s2) n)
        ]
    (- (+ s1-sum s2-sum) together-sum)))

(defn x-83 []
  (big-divide-7 100000000 3 5)
  ;[(ar-sum 3 10) (ar-sum 5 10)]
  )

(defn transitive-closure-1 [tuples]
  (let [abutting-comp (comparator (fn [[ll lr] [rl rr]] (= lr rl)))
        left (set (map first tuples))
        right (set (map second tuples))
        in-common (clojure.set/intersection left right)
        _ (println left right in-common)
        transitives (->> in-common
                         (map (fn [item] (filter (fn [[l r]] (or (= item l) (= item r))) tuples)))
                         (mapv (fn [xs] (sort abutting-comp xs)))
                         probe-on
                         (map (fn [[[ll lr] [rl rr]]] [ll rr]))
                         )]
    (into tuples transitives)))

(def data-1 #{[8 4] [9 3] [4 2] [27 9]})
(def data-2 #{["cat" "man"] ["man" "snake"] ["spider" "cat"]})

(defn x-84 []
  (transitive-closure-1 data-2))

(defn insert-before-idx [v n e]
  (assert (<= n (count v)))
  (if (= n (count v))
    (into v [e])
    (let [l (subvec v 0 n)
          r (subvec v n (count v))]
      (into l (into [e] r)))))

;; If it is a right one that gets a match we need to insert the left one
;; before where the match is. This returns the idx where to insert it and
;; what to insert
(defn whereis [l r xs]
  (let [il (.indexOf xs l)
        ir (.indexOf xs r)]
    (cond
      (= [-1 -1] [il ir]) nil
      (not= -1 il) [(inc il) r]
      (not= -1 ir) [ir l])))

(defn x-87 []
  (let [v ["Baboon" "Bear" "Rabbit"]
        [where what] (whereis "Bear" "Tiger" v)]
    (insert-before-idx v where what)))

(defn combinations [population sz]
  (cond
    (= sz 0) '(())
    (empty? population) '()
    :else (concat (mapv #(cons (first population) %) (combinations (rest population) (dec sz)))
                  (combinations (rest population) sz))))

(defn transitive-closure-2 [tuples]
  (->> tuples
       (reduce (fn [acc [l r]]
                 (let [wheres (map #(whereis l r %) acc)]
                   (if (empty? (remove nil? wheres))
                     (conj acc [l r])
                     (let [updater (->> wheres
                                        (map-indexed #(when (some? %2) (into [%1] %2)))
                                        (some #(when (some? %) %)))
                           where-1 (first updater)
                           where-2 (second updater)
                           what (first (drop 2 updater))]
                       (update acc where-1 (fn [v]
                                             (insert-before-idx v where-2 what)))))))
               [])
       (mapcat #(map vec (combinations % 2)))
       (into #{})))

;; Same but functions included
(defn transitive-closure-3 [tuples]
  (letfn [(combos [pop sz]
            (cond
              (= sz 0) '(())
              (empty? pop) '()
              :else (concat (mapv #(cons (first pop) %) (combos (rest pop) (dec sz)))
                            (combos (rest pop) sz))))]
    (let [whereis (fn [l r xs]
                    (let [il (.indexOf xs l)
                          ir (.indexOf xs r)]
                      (cond
                        (= [-1 -1] [il ir]) nil
                        (not= -1 il) [(inc il) r]
                        (not= -1 ir) [ir l])))
          insert-before-idx (fn [v n e]
                              (if (= n (count v))
                                (into v [e])
                                (let [l (subvec v 0 n)
                                      r (subvec v n (count v))]
                                  (into l (into [e] r)))))
          some? #(-> % nil? not)]
      (->> tuples
           (reduce (fn [acc [l r]]
                     (let [wheres (map #(whereis l r %) acc)]
                       (if (empty? (remove nil? wheres))
                         (conj acc [l r])
                         (let [updater (->> wheres
                                            (map-indexed #(when (some? %2) (into [%1] %2)))
                                            (some #(when (some? %) %)))
                               where-1 (first updater)
                               where-2 (second updater)
                               what (first (drop 2 updater))]
                           (update-in acc [where-1] (fn [v]
                                                      (insert-before-idx v where-2 what)))))))
                   [])
           (mapcat #(map vec (combos % 2)))
           (into #{})))))

(defn x-85 []
  (transitive-closure-3 data-2))

(defn x-86 []
  (insert-before-idx [:a :b :c] 4 :z))

(defn x-87 []
  (mapcat #(combo/combinations % 2) [[8 4 2] [27 9 3]]))

(defn x-88 []
  (combo/combinations [27 9 3] 2))

(defn connected-graph? [tuples]
  (let [whereis (fn [l r xs]
                  (let [il (.indexOf xs l)
                        ir (.indexOf xs r)]
                    (cond
                      (= [-1 -1] [il ir]) nil
                      (not= -1 il) [(inc il) r]
                      (not= -1 ir) [ir l])))
        insert-before-idx (fn [v n e]
                            (if (= n (count v))
                              (into v [e])
                              (let [l (subvec v 0 n)
                                    r (subvec v n (count v))]
                                (into l (into [e] r)))))
        some? #(-> % nil? not)
        tuples->runs (fn [already tuples]
                       (reduce (fn [acc [l r]]
                                 (let [wheres (map #(whereis l r %) acc)]
                                   (if (empty? (remove nil? wheres))
                                     (conj acc [l r])
                                     (let [updater (->> wheres
                                                        (map-indexed #(when (some? %2) (into [%1] %2)))
                                                        (some #(when (some? %) %)))
                                           where-1 (first updater)
                                           where-2 (second updater)
                                           what (first (drop 2 updater))]
                                       (update-in acc [where-1] (fn [v]
                                                                  (insert-before-idx v where-2 what)))))))
                               already
                               tuples))
        grouped (->> tuples
                     ((partial tuples->runs []))
                     (group-by count))
        new-tuples (get grouped 2)
        non-tuple-key (first (disj (set (keys grouped)) 2))
        in-again (get grouped non-tuple-key)
        ]
    (= 1 (count (tuples->runs in-again new-tuples)))))

(def data-1 #{[1 2] [2 3] [3 1] [4 5] [5 6] [6 4]})
(def data-2 #{[:a :b] [:b :c] [:c :d] [:x :y] [:d :a] [:b :e] [:x :a]})

(defn x-89 []
  (connected-graph? data-2))

(defn possible-sums [test]
  (map (fn [xs] (apply + xs)) (mapcat #(combinations test %) (range 1 (-> test count inc)))))

(defn x-90 []
  (let [tests [(set [-1 1 99]) (set [-2 2 888]) (set [-3 3 7777])]]
    (apply clojure.set/intersection (map #(-> % possible-sums set) tests))))

(defn equivalent [& sets]
  (letfn [(combos [pop sz]
            (cond
              (= sz 0) '(())
              (empty? pop) '()
              :else (concat (mapv #(cons (first pop) %) (combos (rest pop) (dec sz)))
                            (combos (rest pop) sz))))]
    (let [possible-sums (fn [test]
                          (map (fn [xs] (apply + xs)) (mapcat #(combos test %) (range 1 (-> test count inc)))))]
      (-> (apply clojure.set/intersection (map #(-> % possible-sums set) sets)) count zero? not))))

(defn x-91 []
  (apply equivalent [(set [-1 1 99]) (set [-2 2 888]) (set [-3 3 7777])]))

;;
;; Good way is to loop-recur, chopping down the w1 and w2 params as go.
;; Will need to look ahead to determine exactly what it is. Actually as we are looking for just
;; one we can do a complete look ahead and short circuit on first difference. The short circuit
;; will return T or F as we only want one letter difference.
;;

(defn chain-2? [w1 w2]
  (loop [[h1 & t1 :as w1] w1
         [h2 & t2 :as w2] w2]
    ;(println "t1 t2 ->" t1 t2)
    ;(println "h1 h2 ->" h1 h2)
    (cond
      (and (not= h1 h2) (= (apply str t1) w2)) :deletion
      (and (= h1 h2) (or t1 t2)) (recur t1 t2)
      (and (= h1 h2) (and (nil? t1) (nil? t2))) :same
      (and (not= h1 h2) (= (apply str w1) (apply str t2))) :insertion
      (and (not= h1 h2) (= (apply str t1) (apply str t2))) :substitution)))

(defn x-92 []
  (apply chain-2? ["cot" "coat"]))

(defn x-93 []
  (apply chain-2? ["coat" "oat"]))

;; S/not ret true as must be only one letter difference
(defn x-94 []
  (chain-2? "hot" "oat"))

(defn x-94a []
  (chain-2? "hot" "hat"))

(defn next-possibilities [x xs]
  (let [chain (fn [w] (when (chain-2? x w) w))]
    [x (keep chain xs)]))

(defn x-95 []
  (next-possibilities "hat" #{"coat" "dog" "cat" "oat" "cot" "hot" "hog"}))

(defn form-map-1 [xs]
  (loop [head (first xs)
         [h & t] (rest xs)
         res []]
    (if (= (count res) (count xs))
      (into {} res)
      (recur h t (conj res (next-possibilities head (remove #{head} xs)))))))

(defn form-map-2 [xs]
  (into {} (reduce (fn [acc ele]
                     (conj acc (next-possibilities ele (remove #{ele} xs))))
                   []
                   xs)))

(def data-1 #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"})
(def data-2 #{"cot" "hot" "bat" "fat"})
(def data-3 #{"to" "top" "stop" "tops" "toss"})

(defn x-95a []
  (form-map-2 data-2))

(defn random-chain-hof [tree]
  (fn []
    (loop [out-chain [(-> tree keys rand-nth)]]
      (if-let [k (->> (get tree (last out-chain))
                      (remove (set out-chain))
                      (#(when (seq %) (rand-nth %))))]
        (recur (conj out-chain k))
        out-chain))))

(defn word-chains [words]
  (let [chained? (fn [w1 w2]
                   (loop [[h1 & t1 :as w1] w1
                          [h2 & t2 :as w2] w2]
                     (cond
                       (and (= h1 h2) (or t1 t2)) (recur t1 t2)
                       (and (not= h1 h2) (= (apply str t1) w2)) :deletion
                       (and (not= h1 h2) (= (apply str w1) (apply str t2))) :insertion
                       (and (not= h1 h2) (= (apply str t1) (apply str t2))) :substitution)))
        next-possibilities (fn [word xs]
                             (let [next-link (fn [w] (when (chained? word w) w))]
                               (keep next-link xs)))
        form-map (fn [xs] (into {} (reduce #(conj %1 [%2 (next-possibilities %2 (remove #{%2} xs))]) [] xs)))
        random-chain-hof (fn [tree]
                           (fn [] (loop [out-chain [(-> tree keys rand-nth)]]
                                    (if-let [k (->> (get tree (last out-chain))
                                                    (remove (set out-chain))
                                                    (#(when (seq %) (rand-nth %))))]
                                      (recur (conj out-chain k))
                                      out-chain))))]
    (->> (repeatedly 100 #((random-chain-hof (form-map words))))
         (group-by count)
         keys
         (apply max)
         (#(= % (count words))))))

(defn x-96 []
  (word-chains data-3))

(defn card-game [trump-suit]
  (fn [cards]
    (let [leading-suit (-> cards first :suit)
          card-comp (comparator (fn [{suit-1 :suit rank-1 :rank} {suit-2 :suit rank-2 :rank}]
                                  (if (not= suit-1 suit-2)
                                    (or (= trump-suit suit-1) (= leading-suit suit-1))
                                    (> rank-1 rank-2))))]
      (->> cards
           (sort card-comp)
           first))))

(def data-1 [{:suit :club :rank 4}
             {:suit :club :rank 9}])
(def data-2 [{:suit :spade :rank 2}
             {:suit :club :rank 10}])
(def data-3 [{:suit :spade :rank 2}
             {:suit :club :rank 10}])
(def data-4 [{:suit :heart :rank 6} {:suit :heart :rank 8}
             {:suit :diamond :rank 10} {:suit :heart :rank 4}])

(defn x-97 []
  ((card-game :heart) data-4))

(def data-1
  ["      "
   " ##   "
   " ##   "
   "   ## "
   "   ## "
   "      "])
(def data-2
  ["     "
   "  #  "
   "  #  "
   "  #  "
   "     "])

(defn game-of-life [universe]
  (let [live-f? (fn [[col row]] (= \# (get-in universe [col row])))
        live-neighbors-count (fn [col row]
                               (let [on-board? (some-fn pos? zero?)]
                                 (->> (for [dx [-1 0 1]
                                            dy [-1 0 1]
                                            :when (not (= 0 dx dy))]
                                        [(+ col dx) (+ row dy)])
                                      (filter (fn [[col row]] (and (on-board? col) (on-board? row))))
                                      (filter live-f?)
                                      count)))
        future-f (fn [live? num-neighbors]
                   (if live?
                     (cond
                       (< num-neighbors 2) \space
                       (> num-neighbors 3) \space
                       :else \#)
                     (if (= 3 num-neighbors)
                       \#
                       \space)))
        row-size (-> universe first count)
        num-rows (count universe)]
    (->> (for [col (range row-size)
               row (range num-rows)]
           (future-f (live-f? [col row]) (live-neighbors-count col row)))
         (partition row-size)
         (mapv (partial apply str)))))

(defn x-98 []
  (game-of-life data-2))

(defn neighbors [col row]
  (let [on-board? (some-fn pos? zero?)]
    (->> (for [dx [-1 0 1]
               dy [-1 0 1]
               :when (not (= 0 dx dy))]
           [(+ col dx) (+ row dy)])
         (filter (fn [[col row]] (and (on-board? col) (on-board? row)))))))

(defn x-99 []
  (neighbors 0 0))

(defn breath-first-search [starting-lab generate-possible-moves destination-state?]
  (loop [already-tested #{starting-lab}
         last-round #{starting-lab}]
    (let [newly-generated (mapcat generate-possible-moves last-round)
          got-there (first (filter destination-state? newly-generated))]
      (if got-there
        (count got-there)
        (let [now-tested (into already-tested newly-generated)]
          (recur now-tested (into #{} (remove already-tested newly-generated))))))))

(defn gen-moves [coll]
  (let [n (last coll)]
    (for [new-num (remove nil? [(* 2 n) (when (even? n) (/ n 2)) (+ n 2)])]
      (conj coll new-num))))

(defn number-maze [start end]
  (let [reached? (fn [curr] (= end (last curr)))
        gen-moves (fn [coll]
                    (let [n (last coll)]
                      (for [new-num (remove nil? [(* 2 n) (when (even? n) (/ n 2)) (+ n 2)])]
                        (conj coll new-num))))
        bfs (fn [starting-lab generate-moves dest-state?]
              (loop [already-tested #{starting-lab}
                     last-round #{starting-lab}]
                (let [newly-generated (mapcat generate-moves last-round)
                      got-there (first (filter dest-state? newly-generated))]
                  (if got-there
                    (count got-there)
                    (let [now-tested (into already-tested newly-generated)]
                      (recur now-tested (into #{} (remove already-tested newly-generated))))))))]
    (if (= start end) 1 (bfs [start] gen-moves reached?))))

(defn x-100 []
  (number-maze 1 1))

(defn graph-tour [graph]
  (let [reached? (fn [curr-edges] (let [num-remaining (count (remove (set curr-edges) graph))]
                                    (or (zero? num-remaining)
                                        (= (inc num-remaining) (count curr-edges)))))
        gen-moves (fn [edges-so-far]
                    (let [abuts? (fn [edge search-edge] (or (= (first search-edge) (second edge))
                                                            (= (second search-edge) (first edge))))
                          ;; Gives us two nodes to see if we can abut against any of the remaining edges
                          search-edge (last edges-so-far)
                          remaining-edges (remove (set edges-so-far) graph)]
                      (if (seq remaining-edges)
                        (for [edge remaining-edges
                              :when (abuts? edge search-edge)]
                          (conj edges-so-far edge))
                        [edges-so-far])))
        bfs (fn [starting-lab generate-moves dest-state?]
              (loop [already-tested #{starting-lab}
                     last-round #{starting-lab}]
                (let [newly-generated (mapcat generate-moves last-round)]
                  (if (first (filter dest-state? newly-generated))
                    true
                    (if (seq newly-generated)
                      (let [now-tested (into already-tested newly-generated)]
                        (recur now-tested (into #{} (remove already-tested newly-generated))))
                      false)))))]
    (bfs [(rand-nth graph)] gen-moves reached?)))

(def data-1 [[:a :b]])
(def data-2 [[:a :a] [:b :b]])
(def data-3 [[:a :b] [:a :b] [:a :c] [:c :a]
             [:a :d] [:b :d] [:c :d]])
(def data-4 [[1 2] [2 3] [3 4] [4 1]])
(def data-5 [[:a :b] [:a :c] [:c :b] [:a :e]
             [:b :e] [:a :d] [:b :d] [:c :e]
             [:d :e] [:c :f] [:d :f]])
(def data-6 [[1 2] [2 3] [2 4] [2 5]])

(defn x-101 []
  (graph-tour data-1))

(def data-a ["adceg" "abcfg"])
(def data-1 ["kitten" "sitting"])

;; https://www.youtube.com/watch?v=b6AGUjqIPsA
(defn levenshtein-distance [source target]
  (let [matrix (atom [])
        get-i-j (fn [i j] (get-in @matrix [i j]))
        set-i-j (fn [i j x] (swap! matrix assoc-in [i j] x))
        formula-set (fn [i j] (let [above (get-i-j (dec i) j)
                                    left (get-i-j i (dec j))
                                    diagonal (get-i-j (dec i) (dec j))]
                                (if (= (get source (dec i)) (get target (dec j)))
                                  (set-i-j i j diagonal)
                                  (set-i-j i j (inc (min above left diagonal))))))
        target-len (-> target count inc)]
    (swap! matrix assoc 0 (vec (range target-len)))
    (doseq [row-num (range 1 (-> source count inc))]
      (swap! matrix assoc row-num [row-num])
      (doseq [col-num (range 1 target-len)]
        (formula-set row-num col-num)))
    (-> @matrix last last)))

(defn x-102 []
  (apply levenshtein-distance data-1))

(defn tic-tc-toe-1 [v]
  (let [all-same? #(-> % frequencies first second ((partial = 3)))
        winner? (fn [co-ords]
                  (or
                    (-> (map first co-ords) all-same?)
                    (-> (map second co-ords) all-same?)
                    (= co-ords [[2 0] [1 1] [0 2]])
                    (= co-ords [[0 0] [1 1] [2 2]])))
        board->winner (fn [board]
                        (->> board
                             (remove (fn [[k _]] (= k :e)))
                             (map (fn [[k co-ords]] [k (winner? co-ords)]))
                             (some (fn [[k b?]] (when b? k)))))
        start-board (->> v
                         (map-indexed (fn [row-num row] (map-indexed (fn [col-num val] [val [col-num row-num]]) row)))
                         (mapcat identity)
                         (group-by first)
                         (map (fn [[k v]] [k (mapv second v)])))
        ]
    (println "co-ords of :es" (-> (filter (fn [[k _]] (= k :e)) start-board) first second))
    (board->winner start-board)))

(defn tic-tc-toe-2 [player v]
  (let [three-same? (fn [co-ords] (-> co-ords frequencies vals (#(apply max %)) ((partial = 3))))
        forward-slash #{[0 2] [1 1] [2 0]}
        back-slash #{[0 0] [1 1] [2 2]}
        winner? (fn [co-ords]
                  (or
                    (-> (map first co-ords) three-same?)
                    (-> (map second co-ords) three-same?)
                    (= forward-slash (clojure.set/intersection forward-slash (set co-ords)))
                    (= back-slash (clojure.set/intersection back-slash (set co-ords)))
                    ))
        board->winner #(->> %
                            (remove (fn [[k _]] (= k :e)))
                            (map (fn [[k co-ords]] [k (winner? co-ords)]))
                            (some (fn [[k b?]] (when b? k))))
        start-board (->> v
                         (map-indexed (fn [row-num row] (map-indexed (fn [col-num val] [val [row-num col-num]]) row)))
                         (mapcat identity)
                         (group-by first)
                         (map (fn [[k v]] [k (mapv second v)]))
                         (into {}))
        empty-spots (-> (filter (fn [[k _]] (= k :e)) start-board) first second)
        ]
    (set (filter (fn [co-ord]
                   (board->winner (probe-on (update start-board player conj co-ord))))
                 empty-spots))))

(defn x-103 []
  (tic-tc-toe-2 :x [[:o :e :e]
                    [:o :x :o]
                    [:x :x :e]]))

(defn x-104 []
  (let [forward-slash #{[0 2] [1 1] [2 0]}
        back-slash #{[0 0] [1 1] [2 2]}
        all-same? #(-> % frequencies probe-on first second ((partial = 3)))
        three-same? (fn [co-ords] (-> co-ords frequencies vals (#(apply max %)) ((partial = 3))))
        winner? (fn [co-ords]
                  ;(println co-ords)
                  (or
                    (-> (map first co-ords) three-same?)
                    (-> (map second co-ords) three-same?)
                    (= forward-slash (clojure.set/intersection forward-slash (set co-ords)))
                    (= back-slash (clojure.set/intersection back-slash (set co-ords)))
                    ))
        board->winner #(->> %
                            (remove (fn [[k _]] (= k :e)))
                            (map (fn [[k co-ords]] [k (winner? co-ords)]))
                            (some (fn [[k b?]] (when b? k))))]
    (board->winner {:o [[0 0] [1 0] [1 2]], :e [[0 1] [0 2] [2 2]], :x [[1 1] [2 0] [2 1] [2 2]]})))

(defn x-105 []
  (let [forward-slash #{[0 2] [1 1] [2 0]}
        back-slash #{[0 0] [1 1] [2 2]}
        ]
    (= forward-slash (clojure.set/intersection forward-slash #{[1 1] [2 0] [2 1] [0 2]}))))

(defn palindromic-split [digits]
  (let [num-digits (count digits)
        side-take-count (quot num-digits 2)
        right (apply str (drop ((if (odd? num-digits)
                                  inc
                                  identity) side-take-count) digits))
        left (apply str (take side-take-count digits))]
    [left right]))

(defn x-105b []
  (palindromic-split "162"))

;; To get the next, even when left and right are not palindromic. Called once only.
;; If palindromic already, s is returned
(defn next-palindromic [s]
  (let [palindromic-split (fn [digits]
                            (let [num-digits (count digits)
                                  side-take-count (quot num-digits 2)
                                  right (apply str (drop ((if (odd? num-digits)
                                                            inc
                                                            identity) side-take-count) digits))
                                  left (apply str (take side-take-count digits))
                                  central (if (odd? num-digits)
                                            (str (first (drop side-take-count digits)))
                                            "")]
                              [left right central]))
        [left right central] (palindromic-split s)
        [extreme-f central] (cond
                              (= "" central) [max central]
                              (= "9" central) [max central]
                              :else [min (-> central Long/parseLong inc str)])]
    (loop [[h-trailing & t-trailing] right
           [h-folded & t-folded] (apply str (reverse left))
           ;; The front is folded under the back, so collection is like that too
           over []
           under []]
      (if h-folded
        (let [trailing-int (Long/parseLong (str h-trailing))
              folded-int (Long/parseLong (str h-folded))]
          (if (not= trailing-int folded-int)
            (let [biggest (first (str (extreme-f trailing-int folded-int)))]
              (recur t-trailing t-folded (conj over biggest) (conj under biggest)))
            (recur t-trailing t-folded (conj over h-trailing) (conj under h-folded))))
        (str (apply str (reverse under))
             central
             (apply str over))))))

;; S/be 171, getting 262
(defn x-105c []
  (next-palindromic "162"))

;; Assumes is given a palindromic number
(defn following-palindromic-1 [s]
  (let [idxs-to-extreme-f (fn [s]
                            (let [abs (fn [val] (if (neg? val) (* -1 val) val))
                                  centre (float (/ (-> s count dec) 2))
                                  m (->> s
                                         (map-indexed (fn [idx c]
                                                        (when (< (Long/parseLong (str c)) 9)
                                                          idx)))
                                         (remove nil?)
                                         (map (juxt identity #(- % centre)))
                                         (group-by #(-> % second abs)))
                                  res (->> (map first (get m (->> m keys (apply min))))
                                           (map #(vector % inc)))]
                              (if (str/index-of s \9)
                                (if (= (str/index-of s \9) (str/last-index-of s \9))
                                  (conj res [(str/index-of s \9) #(- % 9)])
                                  (concat res [[(str/index-of s \9) #(- % 9)] [(str/last-index-of s \9) #(- % 9)]]))
                                res)))]
    (if (every? #(= \9 %) s)
      (str (-> s Long/parseLong inc inc))
      (let [ch-extreme (fn [f]
                         (fn [c] (first (str (f (Long/parseLong (str c)))))))]
        (->> (reduce (fn [s [idx f]]
                       (update s idx (ch-extreme f)))
                     (vec s)
                     (idxs-to-extreme-f s))
             (apply str))))))

;;
;;
;;
(defn following-palindromic-2 [s]
  (if (every? #(= % \9) s)
    (-> s Long/parseLong inc inc str)
    (let [greedy-grab-count (int (Math/ceil (/ (count s) 2)))
          [left right] [(take greedy-grab-count s) (drop greedy-grab-count s)]
          front (->> left (apply str) Long/parseLong inc str)
          back (->> front reverse (drop (- (count left) (count right))) (apply str))]
      (str front back))))

(defn x-108 []
  (->> (iterate following-palindromic-2 "999")
       (map #(Long/parseLong %))
       (take 20)))

(defn x-108a []
  (following-palindromic-2 "9"))

(defn palindromic-1 [n]
  (let [
        ;; Rather than this fn have ch->n, which can just be a lookup table
        xs->int (fn [xs] (Long/parseLong (apply str xs)))
        ch->int {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9}
        int->seq (fn [n]
                   (->> [n 10 []]
                        (iterate (fn [[in mult res]]
                                   (let [remainder (rem in mult)]
                                     [(- in remainder) (* 10 mult) (conj res (quot remainder (/ mult 10)))])))
                        (drop-while #(-> % first pos?)) first last reverse))
        following-f (fn [xs]
                      (if (every? #(= % \9) xs)
                        (-> xs xs->int inc inc str)
                        (let [greedy-grab-count (int (Math/ceil (/ (count xs) 2)))
                              [left right] [(take greedy-grab-count xs) (drop greedy-grab-count xs)]
                              front (->> left xs->int inc str)
                              back (->> front reverse (drop (- (count left) (count right))))]
                          (concat front back))))
        next-f (fn [xs]
                 (let [front-count (int (/ (count xs) 2))
                       front (take front-count xs)
                       reversed-front (reverse front)
                       reversed-back (->> xs reverse (take front-count))
                       central (when (-> xs count odd?)
                                 (first (drop front-count xs)))
                       starter (if (= front reversed-back)
                                 xs
                                 (if (and (-> xs count odd?) (not= \9 central))
                                   (concat front (-> [central] xs->int inc str) reversed-front)
                                   (let [new-front (->> (map (fn [f rb]
                                                               (max (ch->int f) (ch->int rb))) front reversed-back)
                                                        (apply str))]
                                     (if central
                                       (concat new-front [central] (reverse new-front))
                                       (concat new-front (reverse new-front))))))]
                   starter))]
    (->> (iterate following-f (next-f (seq (str n))))
         (map xs->int))))

(defn palindromic-2 [n]
  (let [
        ;; Rather than this fn have ch->n, which can just be a lookup table
        seq->int (fn [xs]
                   (assert (sequential? xs) xs)
                   (if (seq xs)
                     (Long/parseLong (apply str xs))
                     0))
        int->seq (fn [n]
                   (assert (number? n) n)
                   (if (< n 10)
                     [n]
                     (->> [n 10 []]
                          (iterate (fn [[in mult res]]
                                     (let [remainder (rem in mult)]
                                       [(- in remainder) (* 10 mult) (conj res (quot remainder (/ mult 10)))])))
                          (drop-while #(-> % first pos?)) first last reverse)))
        ;; Implement this using loop/recur. Collect the integers using quot/rem as above. When have them know how
        ;; many have gone thru, so mathematically reverse them and add them on the end.
        following-f (fn [n]
                      (assert (number? n) n)
                      (let [xs (int->seq n)]
                        (if (every? #(= % 9) xs)
                          (-> n inc inc)
                          (let [greedy-grab-count (int (Math/ceil (/ (count xs) 2)))
                                [left right] [(take greedy-grab-count xs) (drop greedy-grab-count xs)]
                                front (->> left seq->int inc int->seq)
                                back (->> front reverse (drop (- (count left) (- (count xs) (count left)))))]
                            (seq->int (concat front back))))))
        next-f (fn [n]
                 (assert (number? n) n)
                 (let [xs (int->seq n)
                       front-count (int (/ (count xs) 2))
                       front (take front-count xs)
                       reversed-front (reverse front)
                       reversed-back (->> xs reverse (take front-count))
                       central (when (-> xs count odd?)
                                 (first (drop front-count xs)))]
                   (if (= front reversed-back)
                     xs
                     (if (and (-> xs count odd?) (not= 9 central))
                       (concat front [(-> central inc)] reversed-front)
                       (let [new-front (map (fn [f rb]
                                              (max f rb)) front reversed-back)
                             reversed-new-front (reverse new-front)]
                         (if central
                           (concat new-front [central] reversed-new-front)
                           (concat new-front reversed-new-front)))))))]
    (->> (seq->int (next-f n))
         ((fn [n] n))
         (iterate following-f))))

(defn int->seq-1 [n]
  [(quot n 100) (rem n 100)])

(defn int->seq-2 [n]
  (->> [n 10 []]
       (iterate (fn [[in mult res]]
                  (let [remainder (rem in mult)]
                    [(- in remainder) (* 10 mult) (conj res (quot remainder (/ mult 10)))])))
       (drop-while #(-> % first pos?)) first last reverse))

(defn x-112 []
  (int->seq-2 1))

;; #(quot % 10) grabs all but the last digit
;; #(mod % 10) grabs the last digit
;; #(* % 10) moves the number one to the left
;; To mirror we start off with the number and as we move it to the left we put what we
;; grab from a separate version of it, each time chewing the last digit off
;; If the number is odd we need to have chewed one before we even start.
(defn mirror [[num dig]]
  (loop [l num, r (if (= dig :even) num (quot num 10))]
    (if (= 0 r)
      l
      (recur (+ (* l 10) (rem r 10)), (quot r 10)))))

(defn x-113 []
  (mirror [[1234 :even]]))

(defn init [n]
  (let [left-part (fn [n]
                    (let [s (str n)
                          size (count s)
                          l (subs s 0 (if (odd? size)
                                        (inc (quot size 2))
                                        (quot size 2)))]
                      [(Long/parseLong l)
                       (even? (count l))
                       (long (Math/pow 10 (count l)))]))]
    (left-part n)))

;; Usually just increment, but when goal is reached even will change to odd
(defn increment [[n even? goal]]
  (println "increment" [n even? goal])
  (let [next-n (inc n)]
    (if (= next-n goal)
      [next-n (not even?) (* 10 goal)]
      [next-n even? goal])))

#_(defn x-113a []
    (increment [1234 :even]))

;; answer, right, power. Both start off as n. We grab single digits from right and put them on the end of answer,
;; mathematically.
;; To put on end * by power and add last digit of right
;; right will be chomped down as go, so when its zero we've finished
;; Grabbing r-most is just remainder after divide by 10
;; Numerator of same divide is what we keep of right for next recursion.
;;
(defn mirror [[n even?]]
  (loop [answer n
         right (if even?
                 n
                 (quot n 10))]
    (if (zero? right)
      answer
      (recur (+ (* answer 10)
                (rem right 10))
             (quot right 10)))))

(defn x-114 []
  (->> (iterate increment (init 99755))
       (take 10)
       (map mirror)))

(defn x-115 []
  (mirror [10 true 100])
  ;(mirror [998 false 1000])
  )

(defn palindrome-1 [n]
  (let [init-1 (memoize (fn [n]
                          (let [s (str n)
                                size (count s)
                                even? (even? size)
                                l (subs s 0 (if (not even?)
                                              (inc (quot size 2))
                                              (quot size 2)))]
                            [(Long/parseLong l)
                             even?
                             (long (Math/pow 10 size))])))
        even-digits? #(even? (count %))
        left-middle #(if (even-digits? %)
                       (subs % 0 (quot (count %) 2))
                       (subs % 0 (inc (quot (count %) 2))))
        init-2 #(let [s (left-middle %)]
                  (vector (Long/parseLong s)
                          (even-digits? %)
                          (long (Math/pow 10 (count s)))))
        increment (fn [[n even? goal]]
                    (let [next-n (inc n)]
                      (if (= next-n goal)
                        (if even?
                          [goal false (* 10 goal)]
                          [(quot goal 10) true goal])
                        [next-n even? goal])))
        mirror (fn [[n even?]]
                 (loop [answer n
                        right (if even?
                                n
                                (quot n 10))]
                   (if (zero? right)
                     answer
                     (recur (+ (* answer 10)
                               (rem right 10))
                            (quot right 10)))))]
    (->> (iterate increment (init-1 n))
         (map mirror)
         (filter #(>= % n)))))

(defn palindrome-2 [n]
  (let [to-digits (fn [k] (mapv #(rem (quot k %) 10)
                                (take-while #(< 0 (quot k %)) (iterate #(* 10 %) 1))))
        to-number (fn [k] (apply + (map * k (iterate #(* 10 %) 1))))
        mirror (fn [[num even?]]
                 (let [digits (to-digits num)]
                   (if even? (concat (reverse digits) digits)
                             (concat (reverse (drop 1 digits)) digits))))
        init #(let [left-digits (subvec % (quot (count %) 2))]
                [(to-number left-digits)
                 (even? (count %))
                 (long (Math/pow 10 (count left-digits)))])
        nextp (fn [[num even? goal]]
                (let [m (inc num)]
                  (if (= m goal)
                    (if even?
                      [goal false (* 10 goal)]
                      [(/ goal 10) true goal])
                    [m even? goal])))]
    (->> (iterate nextp (init (to-digits n)))
         (map (comp to-number mirror))
         (filter (partial <= n)))))

(def palindromic palindrome-2)

(defn y-1 []
  (let [stop-w (sw/take-intervals-hof ["explain"])
        res (= (probe-on (take 26 (palindromic 0)))
               [0 1 2 3 4 5 6 7 8 9
                11 22 33 44 55 66 77 88 99
                101 111 121 131 141 151 161])]
    (stop-w 10)
    res))

(defn y-2 []
  (let [stop-w (sw/take-intervals-hof ["explain"])
        res (= (probe-on (take 16 (palindromic 162)))
               [171 181 191 202
                212 222 232 242
                252 262 272 282
                292 303 313 323])]
    (stop-w 10)
    res))

(defn y-3 []
  (let [stop-w (sw/take-intervals-hof ["explain"])
        res (= (probe-off (vec (take 6 (palindromic 1234550000))))
               [1234554321 1234664321 1234774321
                1234884321 1234994321 1235005321])]
    (stop-w 10)
    res))

(defn y-4 []
  (let [stop-w (sw/take-intervals-hof ["explain"])
        res (= (first (palindromic (* 111111111 111111111)))
               (* 111111111 111111111))]
    (stop-w 10)
    res))

(defn y-5 []
  (let [stop-w (sw/take-intervals-hof ["explain"])
        res (= (set (take 199 (palindromic 0)))
               (set (map #(first (palindromic %)) (range 0 10000))))]
    (stop-w 10)
    res))

(defn y-6 []
  (let [stop-w (sw/take-intervals-hof ["explain"])
        res (= true
               (apply < (take 6666 (palindromic 9999999))))]
    (stop-w 10)
    res))

(defn y-7 []
  (let [stop-w (sw/take-intervals-hof ["explain"])
        res (= (nth (palindromic 0) 10101)
               9102019)]
    (stop-w 10)
    res))

(defn y-8 []
  (vec (take 1 (palindromic 162))))

(defn y-9 []
  (sort (distinct (map #(first (palindromic %)) (range 0 10000)))))

(defn y-10 []
  (map #(first (palindromic %)) (range 0 1000)))

(defn palindrome [n]
  (let [to-digits (fn [k] (mapv #(mod (quot k %) 10)
                                (take-while #(< 0 (quot k %)) (iterate #(* 10 %) 1))))
        to-number (fn [k] (apply + (map * k (iterate #(* 10 %) 1))))
        left-digits #(subvec % (/ (count %) 2))
        even-digits? #(even? (count %))
        mirror (fn [[num dig]]
                 (let [digits (to-digits num)]
                   (if (= :even dig) (concat (reverse digits) digits)
                                     (concat (reverse (drop 1 digits)) digits))))
        init #(let [ld (left-digits %)]
                (vector (to-number ld)
                        (if (even-digits? %) :even :odd)
                        (long (Math/pow 10 (count ld)))))
        nextp (fn [[num even goal]]
                (let [m (inc num)]
                  (if (= m goal)
                    (if (= even :even)
                      [goal :odd (* 10 goal)]
                      [(/ goal 10) :even goal])
                    [m even goal])))
        i (init (to-digits n))
        palindromes (iterate nextp i)]
    (filter (partial <= n) (map (comp to-number mirror) palindromes))))
