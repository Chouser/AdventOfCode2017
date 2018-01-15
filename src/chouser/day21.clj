(ns chouser.day21
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]))

(def rules
  (into {}
        (for [line (line-seq (io/reader "resources/input21.txt"))]
          (let [[k [_ & v]] (split-with #{\# \.} (filter #{\# \. \=} line))]
            [(vec k) (vec v)]))))

(def init (vec ".#...####"))

(comment -- for example...
  ;; ;; ;;
  ;; ;; ;;
  ;; ;; ;;
  "cells per grid (g) 36"
  "cells per grid root (gr) 6"
  "cells per mini grid (mg) 4"
  "cells per mini grid root (mgr) 2"
  "mini grids count (mgc) 9"
  "mini grids count root (mgcr) 3")

(defn split-grid [grid]
  (let [gr (long (Math/sqrt (count grid)))
        mgr (+ 2 (rem gr 2))
        mgcr (quot gr mgr)]
    (mapv (fn [mgi]
            (mapv (fn [ci]
                    (grid (+ (* gr mgr (quot mgi mgcr))
                             (* mgr (rem mgi mgcr))
                             (* gr (quot ci mgr))
                             (rem ci mgr))))
                  (range (* mgr mgr))))
          (range (* mgcr mgcr)))))

;;(pprint (split-grid (vec (range 16))))

(defn join-grids [grids]
  (let [mgc  (count grids)
        mgcr (long (Math/sqrt mgc))
        mgr  (long (Math/sqrt (count (grids 0))))]
    (->> (range (* mgr mgc))
         (mapcat #(let [mgi (+ (* mgcr (quot % (* mgcr mgr))) (rem % mgcr))
                        ci (* mgr (rem (quot % mgcr) mgr))]
                    (subvec (grids mgi) ci (+ ci mgr))))
         vec)))

;;(pprint (join-grids (split-grid (vec (range 16)))))

(defn rotate-cw [grid]
  (let [g (count grid)
        gr (long (Math/sqrt g))]
    (mapv #(grid (+ (* gr (- gr 1 (rem % gr))) (quot % gr)))
          (range g))))

;;(pprint (-> (vec (range 9)) rotate-cw rotate-cw rotate-cw rotate-cw))

(defn flip-h [grid]
  (let [g (count grid)
        gr (long (Math/sqrt g))]
    (mapv #(grid (- (* gr (inc (quot % gr))) (inc (rem % gr))))
          (range g))))

;;(pprint (-> (vec (range 9)) flip-h))

(defn apply-rules [mg]
  (reduce (fn [grid f]
            (if-let [out (rules grid)]
              (reduced out)
              (f grid)))
          mg
          [rotate-cw rotate-cw rotate-cw flip-h
           rotate-cw rotate-cw rotate-cw rotate-cw]))

(defn step [grid]
  (->> grid split-grid (mapv apply-rules) join-grids))

(prn :part1 (->> (vec init) step step step step step
                 (filter #{\#}) count))

;; part2. Correct, but takes 8 seconds:
#_
(prn :part2 (->> (vec init)
                 (iterate step)
                 (drop 18)
                 first
                 (filter #{\#}) count))

(defn make-more-rules []
  (reduce
   (fn [rules [k v]]
     (first
      (reduce (fn [[rules grid] f]
                (let [new-grid (f grid)]
                  [(assoc rules new-grid v) new-grid]))
              [rules k]
              [rotate-cw rotate-cw rotate-cw flip-h
               rotate-cw rotate-cw rotate-cw rotate-cw])))
   rules
   rules))

(defn step2 [more-rules grid]
  (->> grid split-grid (mapv more-rules) join-grids))

;; Huh. 5.4 seconds.
(time
 (let [more-rules (make-more-rules)]
   (prn :part2 (->> (vec init)
                    (iterate #(step2 more-rules %))
                    (drop 18)
                    first
                    (filter #{\#}) count))))
