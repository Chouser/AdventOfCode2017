(ns chouser.day20
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]))

(def particles
  (for [line (line-seq (io/reader "resources/input20.txt"))]
    (map vec (partition 3 (map #(Long/parseLong %) (re-seq #"-?\d+" line))))))

(def test-particles
  [[[3,0,0] [2,0,0] [-1,0,0]]
   [[4,0,0] [0,0,0] [-2,0,0]]])

(defn sum-abs [v]
  (reduce + (map #(Math/abs %) v)))

;; Just need min acceleration, right?
(->> particles
     (map-indexed (fn [pnum [p v a :as particle]] {:pnum pnum :p p :v v :a a}))
     (map (fn [{:keys [a] :as particle}] (assoc particle :ad (- (sum-abs a)))))
     (apply max-key :ad)
     :pnum
     prn)
;; right.

;; part 2

(defn step-particle [[p v a]]
  (let [v (mapv + v a)]
    [(mapv + p v) v a]))

(defn remove-collisions [ps]
  (prn (count ps))
  (let [collisions (->> ps
                        (map #(nth % 0))
                        frequencies
                        (keep (fn [[p c]] (when (> c 1) p)))
                        set)]
    (remove #(contains? collisions (nth % 0)) ps)))

;; (pprint (take 4 (iterate #(mapv step-particle %) test-particles)))

#_
(let [ps2 (sort-by #(sum-abs (nth % 2)) particles)]
  (->> (iterate #(remove-collisions (map step-particle %)) ps2)
       (take-while (fn [ps3]
                     (not= ps3 (sort-by sum-abs (map #(nth % 0) ps3)))))
       count
       (prn :howmany)))

;; Seems to stop decreasing at 448 particles ... not the most satisfying solution.
