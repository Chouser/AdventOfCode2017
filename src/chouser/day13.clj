(ns chouser.day13
  (:require [clojure.java.io :as io]))

(def input
  (for [line (line-seq (io/reader "resources/input13.txt"))]
    (map #(Integer/parseInt %) (re-seq #"\d+" line))))

(defn position-ish [time range]
  (if (= range 1)
    0
    (rem time (+ range range -2))))

(defn severity [start-time depth range]
  (if (zero? (position-ish (+ depth start-time) range))
    (* depth range)
    0))

;; (map (fn [[d r]] (severity 0 d r)) [[0 3] [1 2] [4 4] [6 4]])

(prn :part-1 (reduce + (map (fn [[d r]] (severity 0 d r)) input)))

;; part 2:

(prn :part-2
     (some (fn [delay]
             (when (every? (fn [[d r]]
                             (pos? (position-ish (+ d delay) r)))
                           input)
               delay))
           (range)))
