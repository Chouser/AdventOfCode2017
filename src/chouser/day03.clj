(ns chouser.day03)

;; 1 2 10 26

(def min-per-ring
  (concat [2 10]
          (lazy-seq
           (map #(+ %2 8 (- %2 %1)) min-per-ring (rest min-per-ring)))))

;; puzzle 1
(let [input 265149
      rings (take-while #(<= % input) min-per-ring)
      ring-num (count rings)
      offset-in-ring (inc (- input (last rings)))
      ring-section (quot offset-in-ring ring-num)
      offset-in-section (rem offset-in-ring ring-num)
      dist (if (even? ring-section)
             (+ ring-num (- ring-num offset-in-section))
             (+ ring-num offset-in-section))]
  (prn dist)) ;; 438


;; puzzle 2
(loop [grid {[0 0] 1}
       coord [1 0]
       turns (cycle [[0 -1] [-1 0] [0 1] [1 0]])]
  (let [sum (reduce + (for [nx [-1 0 1]
                            ny [-1 0 1]]
                        (get grid (mapv + [nx ny] coord) 0)))
        turns (if (contains? grid (mapv + coord (second turns)))
                turns
                (next turns))]
    (if (< 265149 sum)
      (prn sum) ;; 266330
      (recur (assoc grid coord sum)
             (mapv + coord (first turns))
             turns))))
