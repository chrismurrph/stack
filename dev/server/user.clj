(ns user)

(defn r []
  (require 'user :reload))

(defn x []
  (let [second-highest (fn [nums]
                         (let [highest (apply max nums)
                               without-it (remove #{highest} nums)
                               res (apply max without-it)]
                           res))
        sec-highest (fn [nums]
                      (:second (reduce
                                 (fn [{:keys [highest second] :as acc} ele]
                                   (cond
                                     (> ele highest)
                                     (-> acc
                                         (assoc :highest ele)
                                         (assoc :second highest))

                                     (> ele second)
                                     (-> acc
                                         (assoc :second ele))

                                     :default acc)
                                   )
                                 {:highest 0
                                  :second  0}
                                 nums)))
        str->ints (fn [string]
                    (map #(Integer/parseInt %)
                         (clojure.string/split string #" ")))
        ;input (line-seq (java.io.BufferedReader. *in*))
        input ["1"
               "65 44 55 27"
               "22 45 78 57"]
        all-num-lists (map str->ints (drop 1 input))
        results (map sec-highest all-num-lists)
        _ (println results)]
    (doseq [v results]
      (println (str v))
      )))