(ns fail.pentagonal)

(defn r []
  (require 'user :reload))

(defn x []
  (let [outside-dots (fn [n] (* (dec n) 5))
        two-sides-dots (fn [n] (- (* n 2) 1))
        inside-dots (fn [n] (- (outside-dots n) (two-sides-dots n)))
        count-inner (fn [n] (- n 2))
        all-dots (fn [n]
                   (if (= n 1)
                     1
                     (let [count-outer (outside-dots n)
                           ;_ (println "count-outer " count-outer)
                           num-inners (count-inner n)
                           ;_ (println "num-inners " num-inners)
                           inners (range 2 (+ 2 num-inners))
                           ;_ (println "inners " inners)
                           many-inner-dots-not-double-counted (mapv inside-dots inners)
                           ;_ (println "inners dots " many-inner-dots-not-double-counted)
                           total-inner-dots (reduce + many-inner-dots-not-double-counted)
                           res (+ count-outer total-inner-dots)]
                       res)))
        side-effect (fn [string]
                      (let [num (Integer/parseInt string)
                            all-dots (all-dots num)]
                        (println all-dots)))
        ;input (line-seq (java.io.BufferedReader. *in*))
        ;input ["2"
        ;       "1"
        ;       "2"
        ;       "3"
        ;       "4"
        ;       "5"]
        ;input (into ["10000"] (map str (reverse (range 1 (inc 10000)))))
        ;numbers (mapv #(Long/parseLong %) (rest input))
        _ (with-open [rdr (java.io.BufferedReader. *in*)]
            (doall (map side-effect (drop 1 (line-seq rdr)))))
        ;results (mapv all-dots-memo numbers)
        ]))