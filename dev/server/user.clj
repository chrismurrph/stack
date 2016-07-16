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

(defn hi []
  (println "Hi there"))

(defn rotate-once-back [in-seq times-remaining]
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

(defn rotate-once-forward [in-seq times-remaining]
  (let [first-ele (first in-seq)
        ;; vec needed so that conj adds onto the end
        all-but-first (vec (next in-seq))
        with-on-end (conj all-but-first first-ele)
        new-count (dec times-remaining)]
    (if (zero? new-count)
      with-on-end
      (recur with-on-end new-count))))

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
  (let [no-punc (apply str (remove #{\.\?\!} sentence))
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

(defn permute [s]
  (let [swap (fn [pair] `(~(second pair) ~(first pair)))
        parts (partition 2 s)
        swapped (apply str (flatten (map swap parts)))]
    swapped))

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

;(defn compress-str [s]
;  (let [input (line-seq (java.io.BufferedReader. *in*))
;        s (first input)
;        [h & t] s]
;    (if t
;      (let [sames (take-while #{h} t)
;            contig-count (inc (count sames))]
;        (cond
;          (> contig-count 1) (str h contig-count (compress-str (drop (dec contig-count) t)))
;          (= contig-count 1) (str h (compress-str t))))
;      h)))

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

(defn x
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



