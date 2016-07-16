(ns stack.core
  (:gen-class)
  (:use [clojure.core.async :only [chan go <!! >!! go-loop >! <! close! thread timeout alts!]] :reload)
  (:import (java.lang Character String)))

(defn r [] (require 'stack.core :reload))

;; windows?
;; System.getProperty("os.name")

(defn os-name []
  (System/getProperty "os.name"))

;;
;; Numbers 0-9 ASCII 48 to 57
;; Lowercase letters a-z ASCII 97 to 122
;; Uppercase letters A-Z ASCII 65-90
;;
#_(def lower-asciis (set (map char (range 97 123))))
#_(defn first-lower? [s]
  (when s
    (let [first-letter-str (subs s 0 1)]
      (some lower-asciis first-letter-str))))

(defn first-lower? [s]
  (when s
    (let [first-letter (.charAt s 0)]
      (Character/isLowerCase first-letter))))

(defn mk-first-upper [s]
  (if (> (count s) 0)
    (str (clojure.string/upper-case (.charAt s 0))
         (subs s 1))
    s))

(def some-map {:a 1 :b 2 :c 3})

(def to-rem #{100 102})

(def line-data {100 1 101 2 102 3})

(defn test-remove [to-rem line-data]
  (assert (set? to-rem))
  (assert (map? line-data))
  (into {} (remove (fn [tup] (some #{(first tup)} to-rem)) line-data)))

(defn test-it []
  (test-remove #{} line-data))

(def db (atom [
               {:id 1 :data {:name "Foo"} :par nil}
               {:id 2 :data {:name "Bar"} :par nil}]))

(defn update-par [id value]
  ;; update
  ;; in db atom as defined above
  ;; where :id is equal to id
  ;; set :par to value
  (swap! db (fn [all] (map (fn [item] (if (= (:id item) id) (assoc item :par value) item)) all)))
  )

(defn example []
  (update-par 1 "new value"))

(defn by-id-kw-hof
  "When I looked all projects used 'by-id'. Never-the-less, this is configurable"
  [& config-kw-str]
  (let [all-kws (into #{} config-kw-str)]
    (fn [kw]
      (and (keyword? kw)
           (some #{(name kw)} all-kws)))))

(def by-id? (by-id-kw-hof "by-id" "by-name"))

(defn get-set [kw & args]
  (let [all (into #{} args)]
    (some #{kw} all)))

(def st-vect
  [[:line/by-id 100]
   [:line/by-id 101]
   [:line/by-id 102]
   [:line/by-id 103]])

(defn remove-value [v vect]
  (assert (vector? vect))
  (let [as-set (into #{} vect)
        _ (assert (= (count as-set) (count vect))
                  "Not intended for removing a value where it is not there or > 1 there")]
    (sort (vec (remove #{v} as-set)))))

(def black {:r 0 :g 0 :b 0})
(def white {:r 255 :g 255 :b 255})
(def all-line-colours #{black white})
(def colours-list '(black white))

(defn into-set []
  (into #{} colours-list))

(defn times2 [num] (* 2 num))
(defn square [num] (* num num))
(defn plusfiveall [vec-of-nums] (map #(+ 5 %) vec-of-nums))
(defn toynums
  ([vec-of-nums]
   (plusfiveall vec-of-nums))
  ([vec-of-nums & funcs]
   (let [moremath (apply comp funcs)]
     (plusfiveall (map moremath vec-of-nums)))))

;
; Comes from here:
; https://groups.google.com/forum/#!msg/clojure/VVVa3TS15pU/9WrN_9Mfao4J
;
(def asc compare)

(def desc #(compare %2 %1))

(defn compare-by [[k comp-fn & more] x y]
  "Takes a key and a compare function (these two repeated so in vector), and the two values you want to compare"
  (let [result (comp-fn (k x) (k y))
        ;_ (println "RES: " result " from " x y ", where key: " k)
        ]
    (if (and (zero? result) (seq more))
      (recur more x y)
      result)))

(def data
  {:user1 {:money 400 :sex :male}
   :user2 {:money 300 :sex :female}
   :user3 {:money 200 :sex :male}
   })

(defn richest-by-sex [users s]
  (ffirst (sort #(compare-by [(comp :money val) desc] %1 %2) (filter (comp #{s} :sex val) users))))

(defn transfer [ledger from-id to-id amt]
  (dosync
    (let [from-id (get @ledger from-id)
          to-id (get @ledger to-id)]
      (if from-id
        (if to-id
          (let [bal1 (get from-id :balance)
                bal2 (get to-id :balance)]
            (cond (< amt bal1)
                  (do (alter bal1 - amt)
                      (alter bal2 + amt))
                  :else "Too much")))))))

(defn checkCase [str]
  (let [len (count str)]
    (and (> len 1) (re-matches #"[A-Z]+" str))))

(defn answer [str]
  (count (filter checkCase (.split str " "))))

(def names
  [{:id 1 :name "Bob"}
   {:id 2 :name "Jane"}
   {:id 3 :name "Greg"}])

;;get the :name where :id=3
(defn answer-old []
  (some (fn [e] (when (= 3 (:id e)) (:name e))) names))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Howdy partner!"))
  
(defn say-hi []
  "Hi!")

(def latin '({:a 1 :b "lorem"} {:a 2 :b "ipsum"}))

(defn transform [kw in]
  (zipmap (map kw in) in)
  ;(zipmap (map (comp keyword str kw) in) in)
  )

;(defn my-printer [to-chan what]
;  (go
;    (println "EMITTING: " what)
;    (>! to-chan what)))
;

;(defn show-alot []
;  (let [some-chan (chan)
;        some-printer (partial my-printer some-chan)]
;    (map some-printer (range 10))))
;
;(def printer-chan ((my-printer (chan) (range))))

(def log-chan (chan))

(thread
  (loop []
    (when-let [v (<!! log-chan)]
      (println v)
      (recur)))
  (println "Log Closed"))

(defn close-log-chan []
  (close! log-chan))

(defn log [msg]
  (>!! log-chan msg))

(defn my-sender [to-chan values]
  (go-loop [[x & xs] values]
           (>! to-chan x)
           (when (seq xs) (recur xs))))

(defn my-receiver [from-chan f]
  (go-loop []
           (let [res (<! from-chan)]
             (f res)
             (recur))))

(defn debounce [in ms]
  (let [out (chan)]
    (go-loop [last-val nil
              first-time true]
             (let [val (if (nil? last-val) (<! in) last-val)
                   timer (timeout (if first-time 0 ms))
                   [new-val ch] (alts! [in timer])]
               (condp = ch
                 timer (do (log (str "Timer wins for " val)) (>! out val) (recur nil false))
                 in (do (log (str "In wins for " val)) (recur new-val false)))))
    out))

(defn debounce [in ms]
  (let [out (chan)]
    (go-loop [last-val nil
              first-time true]
             (let [val (if (nil? last-val) (<! in) last-val)
                   timer (timeout (if first-time 0 ms))
                   [new-val ch] (alts! [in timer])]
               (condp = ch
                 timer (do (>! out val) (recur nil false))
                 in (recur new-val false))))
    out))

(defn setup-and-go []
  (let [in (chan)
        ch (debounce in 4000)
        sender (my-sender in (range 10))
        receiver (my-receiver ch #(log %))]))


(defn throttle [f ms]
  (fn [val]
    (f val)))

(def throttled (throttle #(println %) 4000))

(defn print-10-times []
  (doseq [x (range 10)]
    ;(throttled x)
    (log x)))

