(ns user
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.pprint :as pprint]))

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