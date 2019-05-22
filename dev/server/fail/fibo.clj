(ns fail.fibo)

(defn r []
  (require 'user :reload))

(defn x []
  (let [side-effect (fn [limit]
                      (let [res (mod (nth (map first
                                               (iterate
                                                 (fn fib-step [[a b]] [b (+ a b)]) [0N 1])) (Long/parseLong limit))
                                     100000007)]
                        (println (str res))))
        memoize-side (memoize side-effect)
        ;input (line-seq (java.io.BufferedReader. *in*))
        ;input ["2"
        ;       "0"
        ;       "1"]
        ;numbers (map #(Long/parseLong %) (rest input))
        ;results (map fibo-seq numbers)
        _ (with-open [rdr (java.io.BufferedReader. *in*)]
            (doall (map memoize-side (drop 1 (line-seq rdr)))))
        ]))