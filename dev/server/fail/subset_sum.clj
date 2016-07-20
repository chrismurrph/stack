(ns subset-sum)

(defn r []
  (require 'user :reload))

;;
;; Test cases 0->3 are fine, but rest are wrong or timeouts
;;
(defn x []
  (let [subset-sum (fn [examining-line target-test-num]
                     (let [
                           ;low-vals (into [] (filter (fn [n] (<= n target-test-num)) examining-line))
                           ordered-from-largest (reverse (sort examining-line))
                           res (reduce
                                 (fn [{:keys [found? res] :as acc} ele]
                                   (if (= found? :found)
                                     acc
                                     (let [current-sum (apply + res)
                                           latest-sum (+ current-sum ele)
                                           ;_ (println "cf" latest-sum target-test-num)
                                           ]
                                       (if (>= latest-sum target-test-num)
                                         (-> acc
                                             (update :res conj ele)
                                             (assoc :found? :found))
                                         (update acc :res conj ele)))))
                                 {:found? :not-found :res []}
                                 ordered-from-largest)]
                       res))
        str->ints (fn [string]
                    (map #(Integer/parseInt %)
                         (clojure.string/split string #" ")))
        ;lines (line-seq (java.io.BufferedReader. *in*))
        lines ["100"
               "67 24 36 33 64 12 36 77 70 13 25 64 47 32 76 59 67 65 89 26 70 67 76 94 82 13 24 94 71 51 13 76 65 27 88 21 70 24 83 21 85 36 60 41 93 72 17 25 18 31 57 71 78 38 89 94 77 52 53 71 52 50 97 52 44 71 37 47 31 47 58 12 19 83 49 64 67 40 34 52 32 34 58 42 31 40 46 86 79 55 67 68 74 58 100 94 63 60 32 97"
               "1"
               "91"
               ]
        out-count (fn [examining-sz res]
                    (if (= examining-sz (count res)) -1 (count res)))
        examining-line (str->ints (second lines))
        sum-fn (partial subset-sum examining-line)
        test-nums (flatten (map str->ints (drop 3 lines)))
        ;_ (println "test-nums " test-nums)
        results (map (partial out-count (count examining-line)) (map :res (map sum-fn test-nums)))
        ]
    (doseq [v results]
      (println (str v))
      )))