(ns fail.trees)

(defn x []
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
        ;lines [
        ;       "5"
        ;       "3"
        ;       "2 3 1"
        ;       ]
        lines (line-seq (java.io.BufferedReader. *in*))
        valid-bst? (fn [ints]
                     (reduce
                       (fn [{:keys [root failure] :as acc} ele]
                         (let [current nil
                               currents-left (-> current :l :val)
                               currents-right (-> current :r :val)
                               currents-val (:val current)
                               ;_ (println "parent: " parent)
                               ;_ (println "current: " current)
                               ;_ (println "")
                               ]
                           (cond
                             (= failure :failed) acc

                             (nil? currents-val)
                             (assoc acc :root {:val ele})

                             (nil? currents-left)
                             (-> acc
                                 (assoc-in [:current :l] ele))

                             (nil? currents-right)
                             (if (< ele currents-val)
                               (assoc acc :failure :failed)
                               (assoc-in acc [:current :r] ele))

                             (and currents-right currents-left)
                             (do
                               ;(println "current filled, ele is " ele)
                               (if (< ele currents-left)
                                 (assoc acc :failure :failed)
                                 (-> acc
                                     (assoc :parent current)
                                     (assoc :current {:val currents-right}))))
                             )))
                       {:root nil :failure :succeded}
                       ints))
        f (fn [line hdr]
            (let [ints (str->ints line)
                  res (valid-bst? ints)
                  ok? (= (:failure res) :succeded)
                  ;_ (println ok?)
                  ]
              ok?))
        outs (many-line-reader lines f false)
        ]
    (doseq [v (map (fn [out] (if out "YES" "NO")) outs)]
      (println v))))
