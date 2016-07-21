(ns template)

(defn given-func [num lst]
  (let [expanded (mapcat #(repeat num %) lst)]
    (doseq [v expanded]
      (println v))))

(defn recursive-func [list]
  (let [some-func (fn [val]
                    (inc val))]
    (loop [current-lst list acc []]
      (if (seq current-lst)
        (let [head (first current-lst)]
          (recur (rest current-lst) (conj acc (some-func head))))
        acc))))

(defn one-float-per-line-x []
  (let [round (fn [precision d] (let [factor (Math/pow 10 precision)]
                                  (/ (Math/round (* d factor)) factor)))
        some-func (fn [float-num]
                    (inc float-num))
        ;input (line-seq (java.io.BufferedReader. *in*))
        input ["2"
               "0.0000"
               "1.0000"]
        numbers (map #(Float/parseFloat %) (rest input))
        ;_ (println numbers)
        round-it (partial round 4)
        results (map (comp round-it some-func) numbers)
        ]
    (doseq [x results]
      (println x))))

(defn one-num-per-line-x []
  (let [some-func (fn [num]
                    (inc num))
        ;input (line-seq (java.io.BufferedReader. *in*))
        input ["2"
               "0"
               "1"]
        numbers (map #(Long/parseLong %) (rest input))
        results (map some-func numbers)
        ]
    (doseq [x results]
      (println (str x)))))

(defn many-nums-per-line-x []
  (let [some-nums-func (fn [nums]
                         (reduce + nums)
                         )
        str->ints (fn [string]
                    (map #(Integer/parseInt %)
                         (clojure.string/split string #" ")))
        ;input (line-seq (java.io.BufferedReader. *in*))
        input ["1"
               "65 44"
               "22 45"]
        all-num-lists (map str->ints (drop 1 input))
        results (map some-nums-func all-num-lists)]
    (doseq [v results]
      (println (str v))
      )))

(defn grouped-numbers-x []
  (let [str->ints (fn [string]
                    (map #(Integer/parseInt %)
                         (clojure.string/split string #" ")))
        ;;
        ;; Read first as number check
        ;; Reduce over rest of lines. If 1 create a 'containing' vector and put into acc
        ;; If >= 2 create a pair and put it into last vector of 'containing' acc
        ;;
        find-pairs (fn [lines-list]
                     (let [chk-count (first (str->ints (first lines-list)))
                           res (reduce
                                 (fn [acc line-ele]
                                   (let [line-ints (str->ints line-ele)
                                         size (count line-ints)
                                         end-vector-at (dec (count acc))
                                         end-vector (when (> end-vector-at -1) (nth acc end-vector-at))
                                         _ (when (> end-vector-at -1) (assert (vector? end-vector)))
                                         ]
                                     (cond
                                       (= 1 size) (conj acc [])
                                       (>= size 2) (assoc acc end-vector-at (conj end-vector (vec line-ints)))
                                       )))
                                 []
                                 (next lines-list))
                           _ (assert (= (count res) chk-count))]
                       res))
        ;input (line-seq (java.io.BufferedReader. *in*))
        input ["2"
               "3"
               "1 1 4"
               "2 2 4"
               "3 3 4 5"
               "4"
               "1 2 4"
               "2 4 4 4 8"
               "3 6 4"
               "4 8 4"]
        many-pair-groupings (find-pairs input)
        some-predicate? (fn [list]
                          (let [_ (println "here: " list)]
                            (map #(reduce + %) list)))
        answers (map some-predicate? many-pair-groupings)
        output (map #(if % "YES" "NO") answers)]
    (doseq [v output]
      (println v))))

(defn one-set-numbers-one-calc-x []
  (let [some-func (fn [pairs]
                    (let [added-up (reduce + (map first pairs))]
                      (/ added-up 2)))
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
        ;input (line-seq (java.io.BufferedReader. *in*))
        input  '("4"
                 "0 0"
                 "0 1"
                 "1 1"
                 "1 0")
        one-pair-grouping (first (find-pairs input))
        answer (some-func one-pair-grouping)
        result (round 4 answer)
        ]
    (println result)))

(defn alternating-header-x []
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
               "84 9"                                       ;; Threshold is 9. 84 is ignored
               "3 13 6 2 10"]
        ;lines (line-seq (java.io.BufferedReader. *in*))
        some-func (fn [threshold v]
                    (filter #(> % threshold) v))
        f (fn [line hdr]
            (let [threshold (second (str->ints hdr))
                  ints (str->ints line)]
              (some-func threshold ints)))
        outs (many-line-reader lines f false)
        ]
    (doseq [v outs]
      (println (apply str (interpose " " v))))))
