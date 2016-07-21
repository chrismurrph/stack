(ns user)

(defn r []
  (require 'user :reload))

(defn x []
  (let [my-fib-seq (fn [limit]
                     (loop [idx limit acc [0 1]]
                       (let [lst (last acc)
                             nxt-lst (nth acc (- (count acc) 2))]
                         (if (pos? idx)
                           (recur (dec idx) (conj acc (+ lst nxt-lst)))
                           (last acc)))))
        fibonacci-seq (fn [limit]
                        (mod (nth (map first
                                       (iterate
                                         (fn fib-step [[a b]] [b (+ a b)]) [0N 1])) limit)
                             100000007))
        input (line-seq (java.io.BufferedReader. *in*))
        input ["7"
               "0"
               "1"
               "2"
               "3"
               "4"
               "5"
               "6"]
        numbers (map #(Long/parseLong %) (rest input))
        results (map my-fib-seq numbers)
        ]
    (doseq [x results]
      (println (str x)))))