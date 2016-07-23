(ns user)

(defn r []
  (require 'user :reload))

(defn x-1 []
  (let [common-prefix (fn [str1 str2]
                        (loop [acc-idx 0]
                          (let [min-len (min (count str1) (count str2))
                                ;_ (println min-len acc-idx)
                                ]
                            (if (= min-len acc-idx)
                              acc-idx
                              (let [val-at-idx-1 (nth str1 acc-idx)
                                    val-at-idx-2 (nth str2 acc-idx)]
                                (if (= val-at-idx-1 val-at-idx-2)
                                  (recur (inc acc-idx))
                                  acc-idx))))))
        space-out (fn [out] (str (count out) " " out))
        ;[str1 str2] (line-seq (java.io.BufferedReader. *in*))
        [str1 str2] ["kitkat" "kit"]
        idx (common-prefix str1 str2)
        in-common (subs str1 0 idx)
        end-1 (subs str1 idx)
        end-2 (subs str2 idx)
        ]
    (println (space-out in-common))
    (println (space-out end-1))
    (println (space-out end-2))))

(defn x-2 []
  (let [some-nums-func (fn [nums-a nums-b]
                         (- nums-a nums-b)
                         )
        str->ints (fn [string]
                    (map #(Integer/parseInt %)
                         (clojure.string/split string #" ")))
        ;input (line-seq (java.io.BufferedReader. *in*))
        input ["5 6 7"
               "3 6 10"]
        [first second] (map str->ints input)
        calc-results (map some-nums-func first second)
        results (reduce (fn [[a b] ele]
                          (cond
                            (pos? ele) [(inc a) b]
                            (neg? ele) [a (inc b)]
                            :default [a b]
                            )
                          )
                        [0 0]
                        calc-results)
        ]
    (let [[a b] results]
      (println (str a " " b))
      )))

(defn x-4 []
  (let [abs (fn [val] (if (neg? val) (* -1 val) val))
        str->ints (fn [string]
                    (map #(Integer/parseInt %)
                         (clojure.string/split string #" ")))
        primary-diag (fn [matrix]
                       (reduce
                         (fn [acc ele]
                           (let [idx (count acc)]
                             (conj acc (nth ele idx))))
                         []
                         matrix))
        ;input (line-seq (java.io.BufferedReader. *in*))
        input ["3"
               "11 2 4"
               "4 5 6"
               "10 8 -12"]
        all-num-lists (map str->ints (drop 1 input))
        primy (reduce + (primary-diag all-num-lists))
        ;_ (println primy)
        secry (reduce + (primary-diag (map reverse all-num-lists)))
        ;_ (println secry)
        res (abs (- primy secry))
        ]
    (println (str res))))

(defn x-5 []
  (let [str->ints (fn [string]
                    (map #(Integer/parseInt %)
                         (clojure.string/split string #" ")))
        ;input (line-seq (java.io.BufferedReader. *in*))
        input ["6"
               "-4 3 -9 0 4 1"]
        line-of-numbers (first (map str->ints (rest input)))
        ;_ (println line-of-numbers)
        results (reduce (fn [{:keys [pos neg zero] :as acc} ele]
                          (cond
                            (pos? ele) (update acc :pos inc)
                            (neg? ele) (update acc :neg inc)
                            :default (update acc :zero inc)
                            )
                          )
                        {:pos 0 :neg 0 :zero 0}
                        line-of-numbers)
        denominator (count line-of-numbers)
        pos (float (/ (:pos results) denominator))
        neg (float (/ (:neg results) denominator))
        zero (float (/ (:zero results) denominator))
        ]
    (println pos)
    (println neg)
    (println zero)))

(defn x []
  (let [
        give-me (fn [n x] (apply str (repeat n x)))
        ;input (line-seq (java.io.BufferedReader. *in*))
        input ["6"]
        num-in (Integer/parseInt (first input))
        ;output [(give-me 5 "#")]
        output (reduce
                 (fn [acc ele]
                   (let [num-spaces (- num-in ele)
                         num-bricks ele
                         ;_ (println num-spaces num-bricks)
                         ]
                     (conj acc (concat (give-me num-spaces " ") (give-me num-bricks "#")))))
                 []
                 (range 1 (inc num-in)))
        ]
    (doseq [x output]
      (println (apply str x)))))