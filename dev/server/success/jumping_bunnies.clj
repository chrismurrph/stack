(ns success.jumping-bunnies)

(defn r []
  (require 'jumping-bunnies :reload))

(defn x []
  (letfn [(lcm [& numbers]
            (let [gcd-fn (fn [a b] (if (zero? b)
                                     a
                                     (recur b (mod a b))))
                  lcm-inner (fn [num1 num2]
                              (let [multiplied (* num1 num2)
                                    gcd (gcd-fn num1 num2)
                                    res (/ multiplied gcd)]
                                res))
                  [head & tail] numbers]
              (if (nil? tail)
                head
                (lcm-inner head (apply lcm tail)))))]
    (let [str->ints (fn [string]
                      (map #(Integer/parseInt %)
                           (clojure.string/split string #" ")))
          ;input (line-seq (java.io.BufferedReader. *in*))
          ;; 990 supposed answer
          input ["5"
                 "10 11 2 18 10"]
          all-num-lists (map str->ints (drop 1 input))
          results (map #(apply lcm %) all-num-lists)
          ]
      (doseq [v results]
        (println (str v))
        ))))