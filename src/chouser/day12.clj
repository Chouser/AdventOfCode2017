(ns chouser.day12
  (:require [clojure.java.io :as io]))

(def pipes-from
  (into {}
        (for [line (line-seq (io/reader "resources/input12.txt"))]
          (let [[x & ys] (map #(Integer/parseInt %) (re-seq #"\d+" line))]
            [x ys]))))

;; part 1
(loop [[x & xs] [0]
       visited #{}]
  (if (nil? x)
    (count visited)
    (recur
     (concat xs (remove visited (pipes-from x)))
     (conj visited x))))


;; part 2
(defn connected-to [start]
  (loop [[x & xs] [start]
         visited #{}]
    (if (nil? x)
      visited
      (recur
       (concat xs (remove visited (pipes-from x)))
       (conj visited x)))))

(prn :part2 (count (set (map connected-to (keys pipes-from)))))

