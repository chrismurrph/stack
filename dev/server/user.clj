(ns user)

(defn r []
  (require 'user :reload))

(comment
  (if (> decimal-number doing-num)
    (if (nil? remainder)
      (let [div-res (quot decimal-number doing-num)
            div-remain (rem decimal-number doing-num)]
        (recur (rest current-lst) (conj acc div-res) div-remain))
      (recur (rest current-lst) (conj acc (quot remainder doing-num)) remainder))
    (recur (rest current-lst) acc remainder)))

(defn decimal->base-converter [base starting-power]
  (letfn [(recursive-func [list target-number]
            (loop [current-lst list acc [] remainder nil]
              (if (seq current-lst)
                (let [head (first current-lst)
                      doing-num (Math/pow base head)]
                  (if (> doing-num target-number)
                    ;; Keep going till we are inside a boundary
                    (recur (rest current-lst) acc remainder)
                    (if remainder
                      (let [calc-res (quot remainder doing-num)
                            calc-remain (rem remainder doing-num)]
                        (when (> calc-res (dec base))
                          (println "BAD" calc-res remainder))
                        (recur (rest current-lst) (conj acc calc-res) calc-remain))
                      (let [div-res (quot target-number doing-num)
                            div-remain (rem target-number doing-num)
                            _ (println "doing" doing-num "down to" head "for" target-number)
                            _ (when (> div-res (dec base))
                                (println "BAD" div-res remainder))]
                        (recur (rest current-lst) (vec (concat acc [div-res div-remain])) div-remain)))))
                acc)))]
    (fn [decimal-number]
      (let [powers (reverse (range 1 (inc starting-power)))
            _ (println powers)
            start-at (Math/pow base starting-power)
            _ (assert (> start-at decimal-number))
            res (recursive-func powers decimal-number)]
        res))))

(defn digits->special-number [digits]
  (reduce (fn [a b] (+ (* a 6) b)) 0 digits))

(defn digits-fn [n]
  (->> n str (map (comp read-string str))))

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