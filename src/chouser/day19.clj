(ns chouser.day19
  (:require [clojure.java.io :as io]))

(def grid
  (into [] (line-seq (io/reader "resources/input19.txt"))))

(defn part1 []
  (loop [coord [0 (count (take-while #{\space} (grid 0)))]
         delta [1 0]
         found []]
    (let [c (get-in grid coord)]
      (cond
        (= \space c) (apply str found)
        (#{\- \|} c) (recur (mapv + coord delta) delta found)
        (<= (int \A) (int c) (int \Z)) (recur (mapv + coord delta) delta (conj found c))
        (= c \+) (let [d1 [(delta 1) (delta 0)]
                       d2 (mapv * d1 [-1 -1])
                       [new-delta] (remove #(= \space (get-in grid (mapv + coord %)))
                                           [d1 d2])]
                   (recur (mapv + coord new-delta) new-delta found))))))

(prn :part1 (part1))

(defn part2 []
  (loop [coord [0 (count (take-while #{\space} (grid 0)))]
         delta [1 0]
         steps 0]
    (let [c (get-in grid coord)]
      (cond
        (= \space c) steps
        (= c \+) (let [d1 [(delta 1) (delta 0)]
                       d2 (mapv * d1 [-1 -1])
                       [new-delta] (remove #(= \space (get-in grid (mapv + coord %)))
                                           [d1 d2])]
                   (recur (mapv + coord new-delta) new-delta (inc steps)))
        :else (recur (mapv + coord delta) delta (inc steps))))))

(prn :part2 (part2))
