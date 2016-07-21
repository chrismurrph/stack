(ns user)

(defn r []
  (require 'user :reload))

(defn decimal->base-converter [base starting-power]
  (letfn [(recursive-func [list target-number]
            (loop [current-lst list acc [] remainder nil]
              (if (seq current-lst)
                (let [head (first current-lst)
                      boundary-num (Math/pow base head)]
                  (if (> boundary-num target-number)
                    ;; Keep going till we are inside a boundary
                    (recur (rest current-lst) acc remainder)
                    (if remainder
                      (let [calc-res (quot remainder boundary-num)
                            calc-remain (rem remainder boundary-num)
                            ]
                        (recur (rest current-lst) (conj acc calc-res) calc-remain))
                      (let [div-res (quot target-number boundary-num)
                            div-remain (rem target-number boundary-num)]
                        (recur (rest current-lst) (vec (conj acc div-res)) div-remain)))))
                (conj acc remainder))))]
    (fn [decimal-number]
      (let [powers (reverse (range 1 (inc starting-power)))
            _ (println powers)
            start-at (Math/pow base starting-power)
            _ (assert (> start-at decimal-number))
            res (recursive-func powers decimal-number)]
        res))))

(defn char-translator [n]
  (cond
    (= n 0) 0
    (= n 1) \a
    (= n 2) \t
    (= n 3) \l
    (= n 4) \s
    (= n 5) \i
    (= n 6) \n
    ))

(defn x-test []
  (char-translator 0))

(defn x []
  (let [
        ;large [7 7 9 2 8 7 5]
        ;the-digits (digits->special-number large)
        ;_ (println "S/be in base 7:" the-digits)
        conv (decimal->base-converter 7 9)
        res2 (conv 7792875)
        _ (println res2)
        res3 (map (comp char-translator int) res2)
        ]
    (->> res3 (apply str))
    ))