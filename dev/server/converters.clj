(ns converters)

(defn decimal->base-converter [base starting-power]
  (fn [decimal-number]
    (let [start-at (Math/pow base starting-power)]
      (assert (> start-at decimal-number)))))
