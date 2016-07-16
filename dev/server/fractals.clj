(ns fractals
  (:require [utils :as u]))

(defn margin [inner outer]
  (let [
        ;_ (println "inner: " inner "outer: " outer)
        _ (assert (>= outer inner) (str inner " not less than " outer))
        diff (- outer inner)]
    (quot diff 2)))

(seq? [])

(defn foo [& bar] (if (apply = bar 12) 1 2))

(defn outside-triangles-square? [{:keys [loc-row loc-col] :as location}
                                 {:keys [row col width height] :as triangle}]
  (cond
    (< loc-row row) true
    (>= loc-row (+ row height)) true
    (< loc-col col) true
    (>= loc-col (+ col width)) true
    :default false))

;;
;; Depending on which row you are on there will be more columns printed
;; To world is from col to (col + width) columns
;; From world is rows - at the biggest row we want to see max num colums returned
;;
(defn triangle-pixel-on? [{:keys [loc-row loc-col] :as location}
                          {:keys [row col width height] :as triangle}]
  (if (outside-triangles-square? location triangle)
    false
    (let [from-world {:min row :max (+ row (dec height))}
          ;_ (println "loc-row: " loc-row)
          ;_ (println "from: " from-world)
          to-world {:min col :max (+ col (dec width))}
          ;_ (println "to: " to-world)
          on-count (u/scale from-world to-world loc-row)
          on-count (if (= 0 on-count) 1 on-count)
          ;_ (println "at top: " on-count)
          indent (margin on-count width)
          start (+ col indent)
          end (+ col indent on-count)
          between? (and (>= loc-col start) (< loc-col end))
          ]
      between?)))

;;
;; As soon as is on one we break out
;;
(defn triangle-pixel-on-many? [triangles location]
  (let [on-fn? (partial triangle-pixel-on? location)
        on? (some on-fn? triangles)]
    on?))

(defn triangles-within [{:keys [row col width height] :as triangle}]
  (let [small-width (u/divide width 2)
        small-height (u/divide height 2)
        default {:width small-width :height small-height}
        top (assoc default
              :row row
              :col (u/divide (+ col width) 4))
        lower-left (assoc default
                     :row (u/divide (+ row height) 2)
                     :col (+ col 0))
        lower-right (assoc default
                      :row (u/divide (+ row height) 2)
                      :col (u/divide (+ col width) 2))]
    {:top top :lower-left lower-left :lower-right lower-right}))

(defn create-all [depth triangle]
  (let [triangles (triangles-within triangle)
        dep (dec depth)]
    (if (pos? dep)
      (flatten (remove nil? [(create-all dep (:top triangles))
                             (create-all dep (:lower-left triangles))
                             (create-all dep (:lower-right triangles))]))
      (list triangle))))

(defn draw [triangles]
  (let [on? (partial triangle-pixel-on-many? triangles)]
    (doseq [row (range 32)]
      (doseq [col (range 63)]
        (let [nl? (= col 62)
              draw? (on? {:loc-col col :loc-row row})
              what (if draw? "1" "_")]
          (print what)
          (when nl?
            (print "\n")))))))

(defn x []
  (let [triangles (create-all 3 {:row 0 :col 0 :width 63 :height 32})
        _ (println triangles)]
    (draw triangles)))

(defn x-25 []
  (let [tri {:width 31, :height 16, :row 0, :col 15}
        loc {:loc-col 23 :loc-row 0}
        res? (triangle-pixel-on? loc tri)]
    res?))

(defn x-24 []
  (create-all 2 {:row 0 :col 0 :width 63 :height 32}))
