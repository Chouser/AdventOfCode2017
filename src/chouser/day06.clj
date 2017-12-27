(ns chouser.day06)

(def bank [14	0	15	12	11	11	3	5	1	6	8	4	9	1	8	4])

(defn puzzle-a [bank]
  (loop [step 0
         bank bank
         seen-banks #{}]
    (if (contains? seen-banks bank)
      step
      (let [max-blocks (apply max bank)
            idx-max (count (take-while #(not= % max-blocks) bank))
            target-idxs (take max-blocks (drop (inc idx-max) (cycle (range (count bank)))))
            new-bank (reduce (fn [bank idx] (update bank idx inc))
                             (assoc bank idx-max 0)
                             target-idxs)]
        (recur (inc step) new-bank (conj seen-banks bank))))))

(prn (puzzle-a bank))


(defn puzzle-b [bank]
  (loop [step 0
         bank bank
         bank-first-seen {}]
    (if (contains? bank-first-seen bank)
      (- step (get bank-first-seen bank))
      (let [max-blocks (apply max bank)
            idx-max (count (take-while #(not= % max-blocks) bank))
            target-idxs (take max-blocks (drop (inc idx-max) (cycle (range (count bank)))))
            new-bank (reduce (fn [bank idx] (update bank idx inc))
                             (assoc bank idx-max 0)
                             target-idxs)]
        (recur (inc step) new-bank (assoc bank-first-seen bank step))))))

(prn (puzzle-b bank))
