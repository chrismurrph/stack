(ns user
  (:require
    [clojure.java.io :as io]
    [clojure.pprint :refer (pprint)]
    [clojure.stacktrace :refer (print-stack-trace)]
    [clojure.tools.namespace.repl :refer [disable-reload! refresh clear set-refresh-dirs]]
    [taoensso.timbre :refer [info set-level!]]
    [utils :as u]
    [clojure.string :as string]
    [clojure.string :as s]))

(defn r []
  (require 'user :reload))

;;SERVER

;(set-refresh-dirs "dev/server")

(set-level! :info)

(defn rotate [how-many in-seq]
  (let [rotate-once-back (fn [in-seq times-remaining]
                           (let [last-ele (last in-seq)
                                 ;_ (println last-ele)
                                 all-but-last (subvec (vec in-seq) 0 (dec (count in-seq)))
                                 ;_ (println all-but-last)
                                 joined-together (concat [last-ele] all-but-last)
                                 new-count (inc times-remaining)
                                 ]
                             (if (zero? new-count)
                               joined-together
                               (recur joined-together new-count))
                             ))
        rotate-once-forward (fn [in-seq times-remaining]
                              (let [first-ele (first in-seq)
                                    ;; vec needed so that conj adds onto the end
                                    all-but-first (vec (next in-seq))
                                    with-on-end (conj all-but-first first-ele)
                                    new-count (dec times-remaining)]
                                (if (zero? new-count)
                                  with-on-end
                                  (recur with-on-end new-count))))]
    (cond
      (pos? how-many) (rotate-once-forward in-seq how-many)
      (neg? how-many) (rotate-once-back in-seq how-many)
      :default in-seq)))

(defn x-1 []
  (rotate -2 [1 2 3 4 5]))

(defn intersperse [xs n]
  (let [in-groups (vec (partition n xs))
        res (apply map vector in-groups)]
    res))

(defn x-2 []
  (intersperse [1 2 3 4 5 6] 2))

(defn split-by-type [xs]
  (let [splits (reduce
                 (fn [acc ele]
                   (let [is-of (type ele)
                         existing (vec (get acc is-of))]
                     (assoc acc is-of (conj existing ele))))
                 {}
                 xs)
        res (into #{} (concat (vals splits)))]
    res))

(defn x-3 []
  (split-by-type [1 :a 2 :b 3 :c]))

(defn occurances [xs]
  (let [as-set (into #{} xs)
        res (map (fn [x]
                   (let [only (filter #(= x %) xs)
                         size (count only)
                         ]
                     [x size])) as-set)]
    (into {} res)))

(defn x-4 []
  (occurances [1 1 2 3 2 1 1]))

(defn rm-dups-1 [xs]
  (let [as-set (into #{} xs)]
    (vec as-set)))

(defn rm-dups [xs]
  (reduce
    (fn [acc ele]
      (let [already (some #{ele} acc)]
        (if already
          acc
          (conj acc ele))))
    []
    xs))

(defn x-5 []
  (rm-dups [1 2 1 3 1 2 4]))

(defn my-comp-test [& fns]
  (apply comp fns))

(defn my-comp [& fns]
  (fn [args]
    (reduce
      (fn [acc f]
        (let [;_ (println "To apply to " acc)
              res (apply f [acc])
              ;_ (println "RES: " res)
              ]
          res))
      args
      (reverse fns))))

(defn x-6 []
  ((my-comp rest reverse) [1 2 3 4]))

(defn x-7 []
  ((my-comp (partial + 3) second) [1 2 3 4]))

;; ((__ zero? #(mod % 8) +) 3 5 7 9)

(defn x-8 []
  ((my-comp zero? #(mod % 8) +) 3 5 7 9))

;;(= "HELLO" ((__ #(.toUpperCase %) #(apply str %) take) 5 "hello world"))
(defn x-9 []
  ((my-comp #(.toUpperCase %) #(apply str %) take) 5 "hello world"))

(defn my-partition [n xs]
  (if (>= (count xs) n)
    (concat
      (list (take n xs))
      (if (> (count xs) n)
        (my-partition n (drop n xs))
        '()))
    '()))

(defn x-10 []
  (my-partition 3 (range 8)))

(defn my-juxt [& fns]
  (fn [& args]
    (mapv #(apply % args) fns)))

(defn x-11 []
  ((my-juxt + max min) 2 3 5 1 6 4))

(defn sort-words [sentence]
  (let [no-punc (apply str (remove #{\. \? \!} sentence))
        words (clojure.string/split no-punc #" ")
        sorted-words (sort-by clojure.string/lower-case words)]
    sorted-words))

(defn x-12 []
  (sort-words "Have a nice day."))

;(defn sieve [s]
;  (cons (first s)
;        (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
;                                 (rest s))))))

(defn x-13 []
  (u/gen-primes 5))

(defn find-type [x]
  (let [count-first (count (first x))]
    (cond
      (= count-first 2) :map
      :default :list)))

(defn x-14 []
  (find-type (range (rand-int 20)) #_{:a 1, :b 2}))

(defn only-perfects-from-str [s]
  (let [str->ints (fn [string]
                    (map #(Integer/parseInt %)
                         (clojure.string/split string #",")))
        numbers (str->ints s)
        sqrt (fn [n]
               (if (> n 0)
                 (Math/sqrt n)
                 0))
        whole-number? (fn [n]
                        (= 0.0 (rem n 1)))
        perfect-square? (fn [n] (-> n sqrt whole-number?))
        perfect-numbers (filter perfect-square? numbers)
        ]
    (apply str (interpose "," perfect-numbers))))

(defn x-15 []
  (only-perfects-from-str "4,5,6,7,8,9"))

(defn x-16 []
  (letfn
    [(foo [x y] #(bar (conj x y) y))
     (bar [x y] (if (> (last x) 10)
                  x
                  #(foo x (+ 2 y))))]
    (trampoline foo [] 1)))

(defn hello-worlds [n]
  (doseq [x (range n)]
    (println "Hello World")))

;(defn repeat [num lst]
;  (let [repeated (mapv (fn [v] (str (repeat v num) "\n")) lst)]
;    (println repeated)))

(defn repeat-times [num lst]
  (let [expanded (mapcat (fn [v] (repeat num v)) lst)]
    (doseq [v expanded]
      (println v))))

(defn x-a [delim lst]
  (let [res (filter #(< % delim) lst)]
    (doseq [v res]
      (println v))))

(defn x-17 []
  (let [lst (line-seq (java.io.BufferedReader. *in*))
        with-idx (map-indexed vector lst)
        filtered (map second (filter (fn [idx-tup] (even? (first idx-tup))) with-idx))]
    (println filtered)))

(defn x-18 [lst]
  (let [with-idx (map-indexed vector lst)
        filtered (map second (filter (fn [idx-tup] (odd? (first idx-tup))) with-idx))]
    filtered))

(defn sum-odd [lst]
  (let [odds (filter odd? lst)]
    (reduce + odds)))

(comment
  (defn e-to-x [x]
    (reduce
      (fn [acc idx]
        (if (zero? idx)
          acc
          (let [powered (Math/pow x idx)
                factorial-of (reduce * (range 1 (+ idx 1)))
                res (/ powered factorial-of)]
            (+ acc res))))
      1
      (range 10))))

(defn x-19 [lst]
  (let [e-to-x (fn [x]
                 (reduce
                   (fn [acc idx]
                     (if (zero? idx)
                       acc
                       (let [powered (Math/pow x idx)
                             factorial-of (reduce * (range 1 (+ idx 1)))
                             res (/ powered factorial-of)]
                         (+ acc res))))
                   1
                   (range 10)))]
    (map e-to-x lst)))

;;
;; Gave up...
;;
(defn in-out []
  (let [round (fn [precision d]
                (let [factor (Math/pow 10 precision)]
                  (/ (Math/round (* d factor)) factor)))
        a-b-formula (fn [n as-and-bs x]
                      (reduce
                        (fn [acc idx]
                          (let [[a b] (nth as-and-bs idx)
                                powered (Math/pow x b)
                                res (* powered a)]
                            (+ acc res)))
                        (range n)))
        str->ints (fn [string]
                    (map #(Integer/parseInt %)
                         (clojure.string/split string #" ")))
        lst (line-seq (java.io.BufferedReader. *in*))
        as-str (first lst)
        as [1 2 3 4 5] #_(str->ints as-str)
        n (count as)
        bs-str (second lst)
        bs [6 7 8 9 10] #_(str->ints bs-str)
        _ (assert (= n (count bs)))
        as-and-bs (map vector as bs)
        third-row-ints (str->ints (nth lst 2))
        left (first third-row-ints)
        right (second third-row-ints)
        increments (map #(round 4 %) (range left right 0.001))
        formula-fn (partial a-b-formula n as-and-bs)
        to-add (map formula-fn increments)
        area (reduce + to-add)
        ]
    (println as-and-bs)
    (println area)
    ))

(defn function? [pairs]
  (let [ins (into #{} (map first pairs))
        grouping-fn (fn [in]
                      (let [outs-for-in (filter #(= in (first %)) pairs)
                            as-set (into #{} (map second outs-for-in))]
                        (count as-set)))
        grouped (group-by grouping-fn ins)
        _ (println "grouped: <" grouped ">")
        count (first (keys grouped))
        ;_ (println "count: <" count ">")
        ]
    (not (> count 1))))

(def input-1
  '("2"
     "3"
     "1 1"
     "2 2"
     "3 3"
     "4"
     "1 2"
     "2 4"
     "3 6"
     "4 8"))

(def input-2 '(
                "1"
                "10"
                "1 0"
                "2 0"
                "3 0"
                "7 0"
                "8 0"
                "9 0"
                "9 1"
                "9 2"
                "9 4"
                "9 5"))

(defn accept-all [input]
  (let [input (line-seq (java.io.BufferedReader. *in*))
        space-str->ints (fn [string]
                          (map #(Integer/parseInt %)
                               (clojure.string/split string #" ")))
        ;;
        ;; Read first as number check
        ;; Reduce over rest. If 1 create a vector and put into acc
        ;; If 2 create a pair and put into last vector of acc
        ;;
        find-pairs (fn [lines-list]
                     (let [chk-count (first (space-str->ints (first lines-list)))
                           res (reduce
                                 (fn [acc line-ele]
                                   (let [line-ints (space-str->ints line-ele)
                                         size (count line-ints)
                                         end-vector-at (dec (count acc))
                                         end-vector (when (> end-vector-at -1) (nth acc end-vector-at))
                                         _ (when (> end-vector-at -1) (assert (vector? end-vector)))
                                         ]
                                     (cond
                                       (= 1 size) (conj acc [])
                                       (= 2 size) (assoc acc end-vector-at (conj end-vector (vec line-ints)))
                                       )))
                                 []
                                 (next lines-list))
                           _ (assert (= (count res) chk-count))]
                       res))
        many-pair-groupings (find-pairs input)
        function? (fn [pairs]
                    (let [ins (into #{} (map first pairs))
                          grouping-fn (fn [in]
                                        (let [outs-for-in (filter #(= in (first %)) pairs)
                                              as-set (into #{} (map second outs-for-in))]
                                          (count as-set)))
                          grouped (group-by grouping-fn ins)
                          ;_ (println "grouped: <" grouped ">")
                          counts (keys grouped)
                          count (apply max counts)
                          ;_ (println "count: <" count ">")
                          ]
                      (not (> count 1))))
        answers (map function? many-pair-groupings)
        output (map (fn [answer] (if answer "YES" "NO")) answers)]
    (doseq [v output]
      (println v))))

(comment (defn x-20 []
           (accept-all input)))

(def input-3 '("4"
                "0 0"
                "0 1"
                "1 1"
                "1 0"))

(defn dist-poly [input]
  (let [input (line-seq (java.io.BufferedReader. *in*))
        round (fn [precision d]
                (let [factor (Math/pow 10 precision)]
                  (/ (Math/round (* d factor)) factor)))
        exp (fn [x pow-of]
              (Math/pow x pow-of))
        sqrt (fn [n]
               (if (> n 0)
                 (Math/sqrt n)
                 0))
        distance (fn [[x1 y1] [x2 y2]]
                   (let [x-delta-squared (exp (- x2 x1) 2)
                         y-delta-squared (exp (- y2 y1) 2)
                         sum-of-differences (+ x-delta-squared y-delta-squared)
                         now-squared (sqrt sum-of-differences)]
                     (round 1 now-squared)))
        space-str->ints (fn [string]
                          (map #(Integer/parseInt %)
                               (clojure.string/split string #" ")))
        ;;
        ;; When only one set of them, with one header
        ;;
        find-pairs (fn [lines-list]
                     (let [res (reduce
                                 (fn [acc line-ele]
                                   (let [line-ints (space-str->ints line-ele)
                                         size (count line-ints)
                                         end-vector-at (dec (count acc))
                                         end-vector (when (> end-vector-at -1) (nth acc end-vector-at))
                                         _ (when (> end-vector-at -1) (assert (vector? end-vector)))
                                         ]
                                     (cond
                                       (= 1 size) (conj acc [])
                                       (= 2 size) (assoc acc end-vector-at (conj end-vector (vec line-ints)))
                                       )))
                                 []
                                 lines-list)]
                       res))
        one-pair-grouping (first (find-pairs input))
        ;_ (println many-pairs)
        answer (reduce
                 (fn [acc ele]
                   (let [{:keys [result last-pair]} acc
                         ;_ (println result last-pair ele)
                         ]
                     (cond
                       (nil? last-pair) {:result 0 :last-pair ele}
                       last-pair (let [dist (distance last-pair ele)]
                                   {:result (+ result dist) :last-pair ele})
                       )))
                 {:result 0 :last-pair nil}
                 one-pair-grouping)
        output (:result answer)
        first-last-dist (distance (first one-pair-grouping) (last one-pair-grouping))
        result (round 4 (+ output first-last-dist))
        ]
    (println result)))

(defn x-21 []
  (dist-poly input-3))

(def input '("4"
              "0 0"
              "0 1"
              "1 1"
              "1 0"))

(defn area-poly [input]
  (let [poly-area (fn [pairs]
                    (let [xs (map first pairs)
                          ys (map second pairs)
                          swap (fn [pair] `(~(second pair) ~(first pair)))
                          abs (fn [val] (if (neg? val)
                                          (* -1 val)
                                          val))
                          paired-xs (vec (partition 2 1 xs))
                          xs-adjout (conj paired-xs (list (last xs) (first xs)))
                          _ (println xs-adjout)
                          paired-ys (vec (map swap (partition 2 1 ys)))
                          ys-adjout (conj paired-ys (list (first ys) (last ys)))
                          _ (println ys-adjout)
                          top-line (mapv (fn [x-pair y-pair]
                                           (let [left (* (first x-pair) (first y-pair))
                                                 right (* (second x-pair) (second y-pair))]
                                             (- left right)))
                                         xs-adjout ys-adjout)
                          added-up (abs (reduce + top-line))
                          ]
                      (/ added-up 2)))
        input (line-seq (java.io.BufferedReader. *in*))
        round (fn [precision d]
                (let [factor (Math/pow 10 precision)]
                  (/ (Math/round (* d factor)) factor)))
        space-str->ints (fn [string]
                          (map #(Integer/parseInt %)
                               (clojure.string/split string #" ")))
        ;;
        ;; When only one set of them, with one header
        ;;
        find-pairs (fn [lines-list]
                     (let [res (reduce
                                 (fn [acc line-ele]
                                   (let [line-ints (space-str->ints line-ele)
                                         size (count line-ints)
                                         end-vector-at (dec (count acc))
                                         end-vector (when (> end-vector-at -1) (nth acc end-vector-at))
                                         _ (when (> end-vector-at -1) (assert (vector? end-vector)))
                                         ]
                                     (cond
                                       (= 1 size) (conj acc [])
                                       (= 2 size) (assoc acc end-vector-at (conj end-vector (vec line-ints)))
                                       )))
                                 []
                                 lines-list)]
                       res))
        one-pair-grouping (first (find-pairs input))
        ;_ (println many-pairs)
        answer (poly-area one-pair-grouping)
        result (round 4 answer)
        ]
    (println result)))

(defn x-22 []
  (area-poly input))

(defn pascals-triangle [n]
  (let [input (line-seq (java.io.BufferedReader. *in*))
        n (Integer/parseInt input)
        pascals-row (fn [n]
                      (letfn [(value-fn [row col]
                                (if (or (= 1 col) (= row col))
                                  1
                                  (+ (value-fn (dec row) (dec col)) (value-fn (dec row) col))))]
                        (map (partial value-fn n) (range 1 (inc n)))))
        res (map pascals-row (range 1 (inc n)))]
    (doseq [v res]
      (println (apply str (interpose " " v))))))

(defn x-23 []
  (pascals-triangle 4))

(defn two-strings []
  (let [input (line-seq (java.io.BufferedReader. *in*))
        [one two] input
        res (interleave one two)]
    (println res)))

(defn x-24 []
  (let [permute (fn [s]
                  (let [swap (fn [pair] `(~(second pair) ~(first pair)))
                        parts (partition 2 s)
                        swapped (apply str (flatten (map swap parts)))]
                    swapped))
        input (next (line-seq (java.io.BufferedReader. *in*)))
        res (map permute input)
        ]
    (doseq [v res]
      (println v))))

(defn x-25
  []
  (let [fibonacci-seq (fn [num]
                        (loop [acc [1 1]
                               ele-at 2]
                          (if (= ele-at (dec num))
                            (last (into [0] acc))
                            (let [prior-val (nth acc (dec ele-at))
                                  prior-prior-val (nth acc (- ele-at 2))
                                  next-val (+ prior-val prior-prior-val)]
                              (recur (conj acc next-val) (inc ele-at))))))
        input (line-seq (java.io.BufferedReader. *in*))
        num (Integer/parseInt (first input))
        res (fibonacci-seq num)
        ]
    (println res)))

(defn x-26 []
  (letfn [(compress-str [strin]
            (let [[h & t] strin]
              (if t
                (let [sames (take-while #{h} t)
                      names-count (count sames)
                      contig-count (inc names-count)]
                  (cond
                    (= contig-count 1)
                    (do
                      (str h (compress-str t)))

                    (> contig-count 1)
                    (let [from (drop names-count t)]
                      (str (str h contig-count) (compress-str from)))
                    ))
                h)))]
    (let [
          input (line-seq (java.io.BufferedReader. *in*))
          ;input "abcaaabbbcc"
          s (first input)
          res (compress-str s)]
      (println res))
    ))

(defn x-27
  []
  (let [sz-at-front (fn [s] (str (count s) " " s))
        common-prefix (fn [str-1 str-2]
                        (loop [acc ""]
                          (let [len (count acc)
                                str-1-done (<= (count str-1) len)
                                str-2-done (<= (count str-2) len)
                                both-done (and str-1-done str-2-done)
                                val-str-1 (when (not str-1-done) (nth str-1 len))
                                val-str-2 (when (not str-2-done) (nth str-2 len))]
                            (cond
                              both-done acc
                              (= val-str-1 val-str-2) (recur (str acc val-str-1))
                              :default acc
                              ))))
        [one two] (line-seq (java.io.BufferedReader. *in*))
        ;one "puppy"
        ;two "puppy"
        prefix (common-prefix one two)
        sz-prefix (count prefix)
        rest-one (subs one sz-prefix)
        rest-two (subs two sz-prefix)
        res [(sz-at-front prefix) (sz-at-front rest-one) (sz-at-front rest-two)]
        ]
    (doseq [v res]
      (println v))))

(defn x-28
  []
  (let [skip-agains (fn [in]
                      (reduce
                        (fn [acc ele]
                          (let [ele-as-str (str ele)]
                            (if (.contains acc ele-as-str)
                              acc
                              (str acc ele))))
                        ""
                        in))
        [one] (line-seq (java.io.BufferedReader. *in*))
        ;one "accabb"
        res (skip-agains one)]
    (println res)))

(defn x-29
  []
  (let [to-out-str (fn [m] (if (:failed m) "False" "True"))
        abs (fn [val]
              (if (neg? val)
                (* -1 val)
                val))
        check-overall-rules (fn [m]
                              (let [r-g-eq (= (:red m) (:green m))
                                    y-b-eq (= (:yellow m) (:blue m))
                                    both-eq (and y-b-eq r-g-eq)]
                                (assoc m :failed (not both-eq))))
        check-prefix-rules (fn [in]
                             (reduce
                               (fn [acc ele]
                                 (if (:failed acc)
                                   acc
                                   (let [ele-as-str (str ele)
                                         colour-kw (case ele-as-str
                                                     "R" :red
                                                     "G" :green
                                                     "Y" :yellow
                                                     "B" :blue)]
                                     (-> acc
                                         (update colour-kw inc)
                                         (assoc :failed (let [r-g-diff (abs (- (:red acc) (:green acc)))
                                                              y-b-diff (abs (- (:yellow acc) (:blue acc)))]
                                                          (or (> r-g-diff 1) (> y-b-diff 1))))))))
                               {:red 0 :green 0 :yellow 0 :blue 0 :failed false}
                               in))
        lines (line-seq (java.io.BufferedReader. *in*))
        ;lines ["4" "RGGR" "RYBG" "RYRB" "YGYGRBRB"]
        res (map (comp check-overall-rules check-prefix-rules) (next lines))
        outs (map to-out-str res)]
    (doseq [v outs]
      (println v))))

;(defn filter-frequent [threshold v]
;  (let [results (keys (filter (fn [kv] (>= (val kv) threshold)) (frequencies v)))
;        order (map #(.indexOf results %) results)
;        both (map vector order results)
;        sorted (sort-by first both)
;        res (mapv second sorted)
;        ]
;    (if (seq res) res [-1])))
;
;(defn x-test-30 []
;  (let [str->ints (fn [string]
;                    (map #(Integer/parseInt %)
;                         (clojure.string/split string #" ")))]
;    (filter-frequent 3 (str->ints "4 3 2 6 1 2 3 4 5 5 5 6 6"))))

(defn x-30 []
  (let [many-line-reader (fn [lines item-fn no-overall-header]
                           (let [seed {:expecting (if no-overall-header :case-header :overall-header)
                                       :results   []}
                                 output (reduce
                                          (fn [acc ele]
                                            (let [nxt-acc (case (:expecting acc)
                                                            :overall-header
                                                            (assoc acc :expecting :case-header)
                                                            :case-header
                                                            (-> acc
                                                                (assoc :current-header ele)
                                                                (assoc :expecting :item))
                                                            :item
                                                            (let [res (item-fn ele (:current-header acc))]
                                                              (-> acc
                                                                  (update :results conj res)
                                                                  (assoc :expecting :case-header)))
                                                            acc)]
                                              nxt-acc))
                                          seed
                                          lines)
                                 res (:results output)]
                             res))
        str->ints (fn [string]
                    (map #(Integer/parseInt %)
                         (clojure.string/split string #" ")))
        lines ["1"
               "84 9"
               "3 6 2 10 3 5 5 6 8 5 8 2 3 9 10 3 7 3 5 3 4 7 6 8 10 6 5 7 6 2 6 2 8 4 7 8 10 6 7 6 5 4 4 6 2 5 7 3 6 7 2 8 6 8 4 2 3 7 4 2 3 6 10 9 9 2 7 10 6 8 9 2 4 9 10 8 8 3 6 8 5 7 8 4"]
        ;lines (line-seq (java.io.BufferedReader. *in*))
        filter-frequent (fn [threshold v]
                          (let [results (keys (filter (fn [kv] (>= (val kv) threshold)) (frequencies v)))
                                ordered (map #(.indexOf v %) results)
                                both (map vector ordered results)
                                ;_ (println both)
                                sorted (sort-by first both)
                                res (mapv second sorted)
                                ]
                            (if (seq res) res [-1])))
        f (fn [line hdr]
            (let [threshold (second (str->ints hdr))
                  ints (str->ints line)]
              (filter-frequent threshold ints)))
        outs (many-line-reader lines f false)
        ]
    (doseq [v outs]
      (println (apply str (interpose " " v))))))

(defn super-digit [n-str]
  (let [digits (fn [number-str] (map #(Character/digit % 10) number-str))
        as-digits (digits n-str)
        ;_ (println as-digits)
        sum (int (reduce + as-digits))
        ;_ (println sum)
        count-of-sum (count (digits (str sum)))
        ;_ (println "count: " count-of-sum)
        ]
    (if (= 1 count-of-sum)
      sum
      (recur (str sum)))))

(defn x-test-31 []
  (super-digit "539539539539539539539539539539"))

(defn x-31 []
  (let [super-digit (fn [n-str] (let [digits (fn [number-str] (map #(Character/digit % 10) number-str))
                                      as-digits (digits n-str)
                                      ;_ (println as-digits)
                                      sum (int (reduce + as-digits))
                                      ;_ (println sum)
                                      count-of-sum (count (digits (str sum)))
                                      ;_ (println "count: " count-of-sum)
                                      ]
                                  (if (= 1 count-of-sum)
                                    sum
                                    (recur (str sum)))))
        line (first (line-seq (java.io.BufferedReader. *in*)))
        ;line "2365978831649490136475575038877779575813226775851820912370812124502641538947920808397703549713678494683928497712437176140282589350277653479225520602813456433277417366680426198633681891184348757007292907409160353745221125354212095528784124728447770959861439390350308313917365021363541712618686942946773324003146008424205688630371656757561012224744901800726911246423272186301595490993253791386102270201965996662707215300748516732223935858816466886068592299708740453558018878677753623653080545592459765998008028026982510689469213738241205802446029154833458048894002646934119082621498341445221491190955459548371083839625590505228681017724678315572531551988758568150699821635779156685637531274097856486075649357610713833072735231599919848220063026429718137766286716343385059699133211699189933339174843625266398503099203416124466032711453854413933737536836406105991857744766344461162222670876732729171585512468615558499979720269427922798431312270483732004392503905160233457811525428432787732543799783309593536386190295516419339222642886780012683583264436427241020490358960438948951090123073035203797984302163150042110707217274102457735317367100133807782064391421012191958312396649052833396257876824943425814834615313474161638240747120342368147351931074481983318414461554116111216672594256301273113776892080967125790153125125885441941114178586071406149630777323200516190208241341822285244325578953416388462284725673478766919050744786263188733438572307443267700831425575113213359873223948072988922668251652320316884761627830570057061821492039968369341602081382603302965910382997241199808824091331180464950187035576778206683245316006405529597170652549163351875206280564448346510252775085876212617353369513619186390565654064068546863018765466029315754619416429621887091818939474391383675337791979997519954871635692656223852981330368145996344325688247632566665645262588764650823901065646663460851602833982853795901687202035893967893724362979886948663369428689585509715442019662325024294581705265808022570365351645495802686891955348084550615538750809287498241260408673517746608582833123696027380353038348218786331710334697053069152326732702246704177127367642287975998486114018970510842276024855162228267767755948030029723573646908844109049374244456580474922058250964260437721278380069311972455077102266167098465788845261016395772392904280411167763443571285141388567074805986331207454652671618663858701085276806486000014124900483188940484497173286284987124699515039831183902383877120136788492339903316093693938833730548247174799553092402671083313285813003903363625219370039413610850308966558948057932583576771492037811460439359542902624599661588624295232252616245186844975995180218889230403968087888108494214819234992007182241562844376961711438491105585390645332161544542326095869642445733823661806228697208277656426326128938546125761166960351482948539114133280810916317873360419836067625116114001377672929333638768292095236809153349426894896869446197395671771932928211109385884940274160388152027885130001908095515987519207099973668415699818805100890690896126345099249836616348702742776559368591004254298734292430313132506862931339852599450961120591169865484036943294045257801193102728400887526383477970523720630900867795111210451854194391002244150921934506879640791680297603712618076174694948300699937409787596860516266369711284028834238022724596540529491965656156574337073988564171171562036245389632730259264953520190321920347601255396137920500496074443380910841069973360586715775083998842187392360751892748665579312212647821816086677544702037289983388842777905728540745718314922321086695691823249801139552886748181144717500999021690749662561227798159566905725946118195067702032931332172088385727012140744362147720922176656204772910422189621466379287662792372374145383317294646977085291873933547226857758530880168793511465268400187943977613894365634645591604177130794105552582447385631392788652188052953414781129655671439788151513996304231024358843023105354157338564270003904673736178109502550650713167258319522545824056524791781105181639237127875356417268878249642171688744070159985867211941494396566216365392991627962886902198712613982017603237174682825814797983967054038269833008728407345912699623004623467287738296110774212005294050799218227654983820235796069236099706911525806244933683548619651925300435846989868138976944006822652462513903624200398184640627878400159480147462063679751851193719499676402496701528971830054482832320057097082360875809145428036636600549270698504540758319126452884313155162288201445118708935758033035083555929081756050975974040793712887964439001525986626643925845343840647344753344394209812309532147589135665638895095150172324875219965305745649692175878476631167339609984047545458299489650202318886162798841051758853022389219423402386031324398337411422060786064174337895040820526914615325075313448800789763344707190242881920892579588963376086146960633381215378914452523108903834607764477580583821712879080194987840450703336371649758616364805184545636558639909806705757141231728890945705281578891764357530862478284617890886615612608017861029342475376559517893073874528735689724989147963227442563141916115837740823088920018306622898786665697436694114134566032298418513134640110497915781256013658157626876007303865007938799121914067544773285391447059337068967640221461662745594510968605338966108977317081902507941847885331172633986644666002196037962414608353479436996966565895587993028389782659415971152403029518493019029205611785920255633270560095438637318885230551138363908343081523545591224134467205438637992783829259919217744713264231649621461535887435410548366408012977973203459983761967831442350892580662433295719596302715165438994052920660204857279403439598616231631708579334309451829396365207336409446940374623441420435439989058506571776629533788131454314121504595268258657172190365178279637763408587826711292793610001999656837923788319438384372260168303662843086203813103771712423574817520332644704407707584630263836497460974781867543909787940278207946350316762434685271126547340568379697114143432270663747926423410939366867323227307888313764100323948708488249291932762190879422500980573314510481474989813161818046596342997489244092851481456635241462079248335760582513488928151459281907438730238065382341691833250917737617330046824951709287812847967496061820848573006896422220098029186444659696393722864425443174804115295661452095886127784785656396807536597522937517010395385281668735072368362229248386090765688300603292905737705424037244946746445640174635429078019229973366378415273692915764515173009646632256826278792193749314522687455317800604524740119271772480341422364037622841158408693597798800732407214231715176216414119458470398433698630492023148912788371335047944731850924533486580463894436416946298869397354691924065211837844914014837056117850937923852135361875895012428779496342389871713463333419295329258828037504468306416370691252321110783303933730724981220411655523128107813025668960480700236419568269112404054054013255954297124592253625509027169128346186086879842249233969016492009074059375814396213470661585889025858173286307393463166771278528359358679107663618852904436981159408575155381452852538538161326799566605463071606829507721627760695695298663348037499346965080571845983286426728562771057921340809114888275207834276278723670038713251819678117637307797126524335451875674322135660001938501046206646725087020563845482789165114015140237476770960542212541262365674096922377749965211055083880951870947588439313087505990968967904452943561444336754539161497904651842954423690339860136920316455021978105823558793189756025331264642326253916951167641064028719315782248353333706214902160610987866723541787809188383034860801960339654982067421224921066723519523759569071993168783868197116308035921525346492767986090665094574147296651701824963729081365854249511700896706324052098265362182297874817162325372836249682033460453106454759294279011517085352648052686287835826956331856000530656816094258663224239727567999798475939204052755892731997428948369450364798425229576414764799300949556003266863462364785971588403993047829919672884685002127952124093077144535644623315342420032186819555104280110883937594970132297831017511120535205810256141119655336117669459771376005411248881994640753601606805393583940281645304757423903447751752383876356426048210439751541893763188890403792485964646286690953432929961087972625329578244854544295899075470411907779181886987220577672224785177419791954783277253421243333498197370278143247403594312138218592254174197047846302000164735966226394070001204323764164912690541799927573761616438820835021411743898829210130296149250887986491553547221899402096605456148489590866922112294624680546281250567297009605663936998939340731539740128008013832941045470722922727419033890751091173021386847464099733717633316304536930447077599861319788266450790785072377536483225439587367122393671167950836029510856465462738804587713268436992128892171509071641161165880608224322539890931445909082786991260734902410993863449929626354001121991398640542747424891140797115509495342486678389565402504229791244504867508301342798992744796454984189142033193018659744179186808244403688375727771580862592404226696538971232713170205127941869698531179001250635892043048835137445218382957976989721755706861701366347894376710532041643279157597459032202591566808167568419301491754821992741943996185368155017334681412500723160011387595363158410560488203993638466615700276165502049790282263414502568166837301414930688269694553727263733690839349714652132206285243796184219815308355634318194592022805870387606542297381666905458451054204419608774066516116961329031388338821535694456637910702328420582124442440139995352494707284230907975581024232726130673375360163257154562557607055131368534412734263401317160921075802949888891100336188281116884460133424076740001934250575466042922149519629860344218277351996828116545149383178424384811799946144322536711027462093434842033767504492071349955677051700618860129870409856113128392373584622061075886499407885141951318807519645386474 100000"
        ;line "148 3"
        ;ints (str->ints line)
        vals (clojure.string/split line #" ")
        n-str (first vals)
        ;_ (println "n-str length: " (count n-str))
        k (Integer/parseInt (second vals))
        ;_ (println "k:" k)
        input (apply str (repeat k n-str))
        ;_ (println "input length: " (count input))
        ;input-int (Integer/parseInt input)
        res (super-digit input)
        ]
    (println (str res))))

(defn x-32 []
  (let [gcd (fn [a b]
              (if (zero? b)
                a
                (recur b (mod a b))))
        str->bigs (fn [string]
                    (map #(bigint %)
                         (clojure.string/split string #" ")))
        ;lines (line-seq (java.io.BufferedReader. *in*))
        lines ["1000"
               "2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631 641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761 769 773 787 797 809 811 821 823 827 829 839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947 953 967 971 977 983 991 997 1009 1013 1019 1021 1031 1033 1039 1049 1051 1061 1063 1069 1087 1091 1093 1097 1103 1109 1117 1123 1129 1151 1153 1163 1171 1181 1187 1193 1201 1213 1217 1223 1229 1231 1237 1249 1259 1277 1279 1283 1289 1291 1297 1301 1303 1307 1319 1321 1327 1361 1367 1373 1381 1399 1409 1423 1427 1429 1433 1439 1447 1451 1453 1459 1471 1481 1483 1487 1489 1493 1499 1511 1523 1531 1543 1549 1553 1559 1567 1571 1579 1583 1597 1601 1607 1609 1613 1619 1621 1627 1637 1657 1663 1667 1669 1693 1697 1699 1709 1721 1723 1733 1741 1747 1753 1759 1777 1783 1787 1789 1801 1811 1823 1831 1847 1861 1867 1871 1873 1877 1879 1889 1901 1907 1913 1931 1933 1949 1951 1973 1979 1987 1993 1997 1999 2003 2011 2017 2027 2029 2039 2053 2063 2069 2081 2083 2087 2089 2099 2111 2113 2129 2131 2137 2141 2143 2153 2161 2179 2203 2207 2213 2221 2237 2239 2243 2251 2267 2269 2273 2281 2287 2293 2297 2309 2311 2333 2339 2341 2347 2351 2357 2371 2377 2381 2383 2389 2393 2399 2411 2417 2423 2437 2441 2447 2459 2467 2473 2477 2503 2521 2531 2539 2543 2549 2551 2557 2579 2591 2593 2609 2617 2621 2633 2647 2657 2659 2663 2671 2677 2683 2687 2689 2693 2699 2707 2711 2713 2719 2729 2731 2741 2749 2753 2767 2777 2789 2791 2797 2801 2803 2819 2833 2837 2843 2851 2857 2861 2879 2887 2897 2903 2909 2917 2927 2939 2953 2957 2963 2969 2971 2999 3001 3011 3019 3023 3037 3041 3049 3061 3067 3079 3083 3089 3109 3119 3121 3137 3163 3167 3169 3181 3187 3191 3203 3209 3217 3221 3229 3251 3253 3257 3259 3271 3299 3301 3307 3313 3319 3323 3329 3331 3343 3347 3359 3361 3371 3373 3389 3391 3407 3413 3433 3449 3457 3461 3463 3467 3469 3491 3499 3511 3517 3527 3529 3533 3539 3541 3547 3557 3559 3571 3581 3583 3593 3607 3613 3617 3623 3631 3637 3643 3659 3671 3673 3677 3691 3697 3701 3709 3719 3727 3733 3739 3761 3767 3769 3779 3793 3797 3803 3821 3823 3833 3847 3851 3853 3863 3877 3881 3889 3907 3911 3917 3919 3923 3929 3931 3943 3947 3967 3989 4001 4003 4007 4013 4019 4021 4027 4049 4051 4057 4073 4079 4091 4093 4099 4111 4127 4129 4133 4139 4153 4157 4159 4177 4201 4211 4217 4219 4229 4231 4241 4243 4253 4259 4261 4271 4273 4283 4289 4297 4327 4337 4339 4349 4357 4363 4373 4391 4397 4409 4421 4423 4441 4447 4451 4457 4463 4481 4483 4493 4507 4513 4517 4519 4523 4547 4549 4561 4567 4583 4591 4597 4603 4621 4637 4639 4643 4649 4651 4657 4663 4673 4679 4691 4703 4721 4723 4729 4733 4751 4759 4783 4787 4789 4793 4799 4801 4813 4817 4831 4861 4871 4877 4889 4903 4909 4919 4931 4933 4937 4943 4951 4957 4967 4969 4973 4987 4993 4999 5003 5009 5011 5021 5023 5039 5051 5059 5077 5081 5087 5099 5101 5107 5113 5119 5147 5153 5167 5171 5179 5189 5197 5209 5227 5231 5233 5237 5261 5273 5279 5281 5297 5303 5309 5323 5333 5347 5351 5381 5387 5393 5399 5407 5413 5417 5419 5431 5437 5441 5443 5449 5471 5477 5479 5483 5501 5503 5507 5519 5521 5527 5531 5557 5563 5569 5573 5581 5591 5623 5639 5641 5647 5651 5653 5657 5659 5669 5683 5689 5693 5701 5711 5717 5737 5741 5743 5749 5779 5783 5791 5801 5807 5813 5821 5827 5839 5843 5849 5851 5857 5861 5867 5869 5879 5881 5897 5903 5923 5927 5939 5953 5981 5987 6007 6011 6029 6037 6043 6047 6053 6067 6073 6079 6089 6091 6101 6113 6121 6131 6133 6143 6151 6163 6173 6197 6199 6203 6211 6217 6221 6229 6247 6257 6263 6269 6271 6277 6287 6299 6301 6311 6317 6323 6329 6337 6343 6353 6359 6361 6367 6373 6379 6389 6397 6421 6427 6449 6451 6469 6473 6481 6491 6521 6529 6547 6551 6553 6563 6569 6571 6577 6581 6599 6607 6619 6637 6653 6659 6661 6673 6679 6689 6691 6701 6703 6709 6719 6733 6737 6761 6763 6779 6781 6791 6793 6803 6823 6827 6829 6833 6841 6857 6863 6869 6871 6883 6899 6907 6911 6917 6947 6949 6959 6961 6967 6971 6977 6983 6991 6997 7001 7013 7019 7027 7039 7043 7057 7069 7079 7103 7109 7121 7127 7129 7151 7159 7177 7187 7193 7207 7211 7213 7219 7229 7237 7243 7247 7253 7283 7297 7307 7309 7321 7331 7333 7349 7351 7369 7393 7411 7417 7433 7451 7457 7459 7477 7481 7487 7489 7499 7507 7517 7523 7529 7537 7541 7547 7549 7559 7561 7573 7577 7583 7589 7591 7603 7607 7621 7639 7643 7649 7669 7673 7681 7687 7691 7699 7703 7717 7723 7727 7741 7753 7757 7759 7789 7793 7817 7823 7829 7841 7853 7867 7873 7877 7879 7883 7901 7907 7919"
               "1000"
               "9973 9967 9949 9941 9931 9929 9923 9907 9901 9887 9883 9871 9859 9857 9851 9839 9833 9829 9817 9811 9803 9791 9787 9781 9769 9767 9749 9743 9739 9733 9721 9719 9697 9689 9679 9677 9661 9649 9643 9631 9629 9623 9619 9613 9601 9587 9551 9547 9539 9533 9521 9511 9497 9491 9479 9473 9467 9463 9461 9439 9437 9433 9431 9421 9419 9413 9403 9397 9391 9377 9371 9349 9343 9341 9337 9323 9319 9311 9293 9283 9281 9277 9257 9241 9239 9227 9221 9209 9203 9199 9187 9181 9173 9161 9157 9151 9137 9133 9127 9109 9103 9091 9067 9059 9049 9043 9041 9029 9013 9011 9007 9001 8999 8971 8969 8963 8951 8941 8933 8929 8923 8893 8887 8867 8863 8861 8849 8839 8837 8831 8821 8819 8807 8803 8783 8779 8761 8753 8747 8741 8737 8731 8719 8713 8707 8699 8693 8689 8681 8677 8669 8663 8647 8641 8629 8627 8623 8609 8599 8597 8581 8573 8563 8543 8539 8537 8527 8521 8513 8501 8467 8461 8447 8443 8431 8429 8423 8419 8389 8387 8377 8369 8363 8353 8329 8317 8311 8297 8293 8291 8287 8273 8269 8263 8243 8237 8233 8231 8221 8219 8209 8191 8179 8171 8167 8161 8147 8123 8117 8111 8101 8093 8089 8087 8081 8069 8059 8053 8039 8017 8011 8009 7993 7963 7951 7949 7937 7933 7927 7919 7907 7901 7883 7879 7877 7873 7867 7853 7841 7829 7823 7817 7793 7789 7759 7757 7753 7741 7727 7723 7717 7703 7699 7691 7687 7681 7673 7669 7649 7643 7639 7621 7607 7603 7591 7589 7583 7577 7573 7561 7559 7549 7547 7541 7537 7529 7523 7517 7507 7499 7489 7487 7481 7477 7459 7457 7451 7433 7417 7411 7393 7369 7351 7349 7333 7331 7321 7309 7307 7297 7283 7253 7247 7243 7237 7229 7219 7213 7211 7207 7193 7187 7177 7159 7151 7129 7127 7121 7109 7103 7079 7069 7057 7043 7039 7027 7019 7013 7001 6997 6991 6983 6977 6971 6967 6961 6959 6949 6947 6917 6911 6907 6899 6883 6871 6869 6863 6857 6841 6833 6829 6827 6823 6803 6793 6791 6781 6779 6763 6761 6737 6733 6719 6709 6703 6701 6691 6689 6679 6673 6661 6659 6653 6637 6619 6607 6599 6581 6577 6571 6569 6563 6553 6551 6547 6529 6521 6491 6481 6473 6469 6451 6449 6427 6421 6397 6389 6379 6373 6367 6361 6359 6353 6343 6337 6329 6323 6317 6311 6301 6299 6287 6277 6271 6269 6263 6257 6247 6229 6221 6217 6211 6203 6199 6197 6173 6163 6151 6143 6133 6131 6121 6113 6101 6091 6089 6079 6073 6067 6053 6047 6043 6037 6029 6011 6007 5987 5981 5953 5939 5927 5923 5903 5897 5881 5879 5869 5867 5861 5857 5851 5849 5843 5839 5827 5821 5813 5807 5801 5791 5783 5779 5749 5743 5741 5737 5717 5711 5701 5693 5689 5683 5669 5659 5657 5653 5651 5647 5641 5639 5623 5591 5581 5573 5569 5563 5557 5531 5527 5521 5519 5507 5503 5501 5483 5479 5477 5471 5449 5443 5441 5437 5431 5419 5417 5413 5407 5399 5393 5387 5381 5351 5347 5333 5323 5309 5303 5297 5281 5279 5273 5261 5237 5233 5231 5227 5209 5197 5189 5179 5171 5167 5153 5147 5119 5113 5107 5101 5099 5087 5081 5077 5059 5051 5039 5023 5021 5011 5009 5003 4999 4993 4987 4973 4969 4967 4957 4951 4943 4937 4933 4931 4919 4909 4903 4889 4877 4871 4861 4831 4817 4813 4801 4799 4793 4789 4787 4783 4759 4751 4733 4729 4723 4721 4703 4691 4679 4673 4663 4657 4651 4649 4643 4639 4637 4621 4603 4597 4591 4583 4567 4561 4549 4547 4523 4519 4517 4513 4507 4493 4483 4481 4463 4457 4451 4447 4441 4423 4421 4409 4397 4391 4373 4363 4357 4349 4339 4337 4327 4297 4289 4283 4273 4271 4261 4259 4253 4243 4241 4231 4229 4219 4217 4211 4201 4177 4159 4157 4153 4139 4133 4129 4127 4111 4099 4093 4091 4079 4073 4057 4051 4049 4027 4021 4019 4013 4007 4003 4001 3989 3967 3947 3943 3931 3929 3923 3919 3917 3911 3907 3889 3881 3877 3863 3853 3851 3847 3833 3823 3821 3803 3797 3793 3779 3769 3767 3761 3739 3733 3727 3719 3709 3701 3697 3691 3677 3673 3671 3659 3643 3637 3631 3623 3617 3613 3607 3593 3583 3581 3571 3559 3557 3547 3541 3539 3533 3529 3527 3517 3511 3499 3491 3469 3467 3463 3461 3457 3449 3433 3413 3407 3391 3389 3373 3371 3361 3359 3347 3343 3331 3329 3323 3319 3313 3307 3301 3299 3271 3259 3257 3253 3251 3229 3221 3217 3209 3203 3191 3187 3181 3169 3167 3163 3137 3121 3119 3109 3089 3083 3079 3067 3061 3049 3041 3037 3023 3019 3011 3001 2999 2971 2969 2963 2957 2953 2939 2927 2917 2909 2903 2897 2887 2879 2861 2857 2851 2843 2837 2833 2819 2803 2801 2797 2791 2789 2777 2767 2753 2749 2741 2731 2729 2719 2713 2711 2707 2699 2693 2689 2687 2683 2677 2671 2663 2659 2657 2647 2633 2621 2617 2609 2593 2591 2579 2557 2551 2549 2543 2539 2531 2521 2503 2477 2473 2467 2459 2447 2441 2437 2423 2417 2411 2399 2393 2389 2383 2381 2377 2371 2357 2351 2347 2341 2339 2333 2311 2309 2297 2293 2287 2281 2273 2269 2267 2251 2243 2239 2237 2221 2213 2207 2203 2179 2161 2153 2143 2141 2137 2131 2129 2113 2111 2099 2089 2087 2083 2081 2069 2063 2053 2039 2029 2027 2017 2011 2003 1999 1997 1993 1987 1979 1973 1951 1949 1933 1931 1913 1907 1901 1889 1879 1877 1873 1871 1867 1861 1847 1831 1823 1811 1801 1789 1787 1783 1777 1759 1753 1747 1741 1733 1723 1721 1709 1699 1697 1693 1669 1667 1663 1657 1637 1627 1621 1619 1613 1609 1607 1601 1597 1583 1579 1571 1567 1559 1553 1549 1543 1531 1523 1511 1499 1493 1489 1487 1483 1481 1471 1459 1453 1451"]
        n-s (str->bigs (second lines))
        ;_ (println (count n-s))
        a (reduce * n-s)
        ;_ (println a)
        m-s (str->bigs (nth lines 3))
        ;_ (println (count m-s))
        b (reduce * m-s)
        ;_ (println b)
        res (mod (gcd a b) 1000000007)
        ]
    ;(println n-s " " a)
    ;(println m-s " " b)
    (println (str res))))

(defn x []
  (let [many-line-reader (fn [lines item-fn no-overall-header]
                           (let [seed {:expecting (if no-overall-header :case-header :overall-header)
                                       :results   []}
                                 output (reduce
                                          (fn [acc ele]
                                            (let [nxt-acc (case (:expecting acc)
                                                            :overall-header
                                                            (assoc acc :expecting :case-header)
                                                            :case-header
                                                            (-> acc
                                                                (assoc :current-header ele)
                                                                (assoc :expecting :item))
                                                            :item
                                                            (let [res (item-fn ele (:current-header acc))]
                                                              (-> acc
                                                                  (update :results conj res)
                                                                  (assoc :expecting :case-header)))
                                                            acc)]
                                              nxt-acc))
                                          seed
                                          lines)
                                 res (:results output)]
                             res))
        str->ints (fn [string]
                    (map #(Integer/parseInt %)
                         (clojure.string/split string #" ")))
        ;lines [
        ;       "5"
        ;       "3"
        ;       "2 3 1"
        ;       ]
        lines (line-seq (java.io.BufferedReader. *in*))
        valid-bst? (fn [ints]
                     (reduce
                       (fn [{:keys [root failure] :as acc} ele]
                         (let [current nil
                               currents-left (-> current :l :val)
                               currents-right (-> current :r :val)
                               currents-val (:val current)
                               ;_ (println "parent: " parent)
                               ;_ (println "current: " current)
                               ;_ (println "")
                               ]
                           (cond
                             (= failure :failed) acc

                             (nil? currents-val)
                             (assoc acc :root {:val ele})

                             (nil? currents-left)
                             (-> acc
                                 (assoc-in [:current :l] ele))

                             (nil? currents-right)
                             (if (< ele currents-val)
                               (assoc acc :failure :failed)
                               (assoc-in acc [:current :r] ele))

                             (and currents-right currents-left)
                             (do
                               ;(println "current filled, ele is " ele)
                               (if (< ele currents-left)
                                 (assoc acc :failure :failed)
                                 (-> acc
                                     (assoc :parent current)
                                     (assoc :current {:val currents-right}))))
                             )))
                       {:root nil :failure :succeded}
                       ints))
        f (fn [line hdr]
            (let [ints (str->ints line)
                  res (valid-bst? ints)
                  ok? (= (:failure res) :succeded)
                  ;_ (println ok?)
                  ]
              ok?))
        outs (many-line-reader lines f false)
        ]
    (doseq [v (map (fn [out] (if out "YES" "NO")) outs)]
      (println v))))
