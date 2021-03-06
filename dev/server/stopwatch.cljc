(ns stopwatch)

(defn now-millisecs []
  #?(:clj (.getTime (java.util.Date.))
     :cljs (.getTime (js/Date.))))

(defn start []
  (let [started-at (now-millisecs)]
    (fn []
      (let [ended-at (now-millisecs)]
        (- ended-at started-at)))))

(defn take-intervals-hof
  "Prints the time taken for each interval of work. Pass to the inner fn a reasonable amount of time,
  beyond which you wish to be informed"
  [interval-names]
  (assert (vector? interval-names))
  (let [last-elapsed (atom 0)
        iter (atom 0)
        elapsed-f (start)]
    (fn [limit]
      (if (>= @iter (count interval-names))
        (println "Too many calls (" (inc @iter) ") to take-intervals-hof inner fn for" interval-names)
        (let [elapsed (elapsed-f)
              diff (- elapsed @last-elapsed)]
          (when (> diff limit)
            (println (nth interval-names @iter) diff "msecs"))
          (reset! last-elapsed elapsed)
          (swap! iter inc)))
      nil)))