(ns example-mapcat)

(defn r []
  (require 'example-mapcat :reload))

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
        repeat-many (fn [dup-times repeat-character character]
                      (if (= (Character/toUpperCase repeat-character) (Character/toUpperCase character))
                        (flatten [character (repeat (dec dup-times) (Character/toUpperCase character))])
                        [character]))
        some-func (fn [dup-by sentence ^Character character]
                    (let [repeater (partial repeat-many dup-by character)]
                      (mapcat #(repeater %) sentence))
                    )
        f (fn [line hdr]
            (let [hdr-str (clojure.string/split hdr #" ")
                  ;_ (println hdr-str)
                  dup-by (Integer/parseInt (second hdr-str))]
              (some-func dup-by line (first (char-array (first hdr-str))))))
        lines ["1"
               "b 4"
               "Below the basement. That's below indeed."
               "o 2"
               "Orthodoxy."]
        ;lines (line-seq (java.io.BufferedReader. *in*))
        outs (many-line-reader lines f false)
        ]
    (doseq [v outs]
      (println (apply str v)))))