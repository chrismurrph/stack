(ns utils)

 (defn rm-punctuation [in-str]
   (apply str (remove #{\.\?\!} in-str)))

(defn abs [val]
  (if (neg? val)
    (* -1 val)
    val))

(defn whole-number? [n]
  (= 0.0 (rem n 1)))

(defn sqrt [n]
  (if (> n 0)
    (Math/sqrt n)
    0))

(defn exp [x pow-of]
  (Math/pow x pow-of))

(defn round [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn divide [num div]
  (let [res (/ num div)
        ;_ (assert (= res (int res)) (str "Got back fraction: " res))
        ]
    (round 0 res)))

;;
;; from-world and to-world are maps of type {:min _ :max _}
;; These max and min are inclusive, so the exact middle when :min 0 and :max 10 is 5
;; Note that we need to do precision-scaling at the end, as there needs to be an exact
;; pixel location where to put circle on the graph
;;
(defn scale [from-world to-world from-val]
  (let [min-from (:min from-world)
        max-from (:max from-world)
        min-to (:min to-world)
        max-to (:max to-world)
        from-diff (- max-from min-from)
        to-diff (- max-to min-to)
        from-proportion (/ (- from-val min-from) from-diff)
        res (* to-diff from-proportion)
        rounded-res (int (Math/ceil res))
        ;_ (println "FROM VAL:" from-val " | RES:" rounded-res " | " res " | F:" from-world " | T:" to-world)
        ]
    rounded-res))

(defn distance [precision [x1 y1] [x2 y2]]
  (let [x-delta-squared (exp (- x2 x1) 2)
        y-delta-squared (exp (- y2 y1) 2)
        sum-of-differences (+ x-delta-squared y-delta-squared)
        now-squared (sqrt sum-of-differences)]
    (round precision now-squared)))

(defn perfect-square? [n]
  (-> n sqrt whole-number?))

(defn output-the-input []
  (let [input (line-seq (java.io.BufferedReader. *in*))]
    (doseq [v input]
      (println v))))

(defn gen-primes [n]
  (letfn [(sieve [s]
            (cons (first s)
                  (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                           (rest s))))))]
    (take n (sieve (iterate inc 2)))))

(defn comma-str->ints
  [string]
  (map #(Integer/parseInt %)
       (clojure.string/split string #",")))

(defn space-str->ints
  [string]
  (map #(Integer/parseInt %)
       (clojure.string/split string #" ")))


