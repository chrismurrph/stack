(ns look-and-say)

(defn r []
  (require 'user :reload))

(defn digits [n]
  (->> n str (map (comp read-string str))))

(defn digits->number [digits]
  (reduce (fn [a b] (+ (* a 10) b)) 0 digits))

(defn look-and-say-once [start]
  (assert (seq? start))
  (let []
    (loop [current-lst start acc []]
      (if (seq current-lst)
        (let [head (first current-lst)
              matching (take-while #(= % head) current-lst)
              how-many (count matching)]
          (recur (drop how-many current-lst) (concat acc [how-many head])))
        acc))))

(defn look-and-say [start n]
  (let [res (loop [start (digits start) n n]
              (if (zero? n)
                start
                (let [
                      ;_ (println start (type start))
                      res (look-and-say-once start)]
                  (recur res (dec n)))))]
    (digits->number res)))

(defn x []
  (look-and-say 221 5))