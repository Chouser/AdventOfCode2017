(ns chouser.day17
  (:require [clojure.pprint :refer [pprint]]))

(defn walk-and-insert [{:keys [coll pos step new-num]}]
  (let [keep-count (inc (rem (+ pos step) (count coll)))]
    {:coll (concat (take keep-count coll)
                   [new-num]
                   (drop keep-count coll))
     :pos keep-count
     :step step
     :new-num (inc new-num)
     }))

;;;; 0  5  2  4  3 (6) 1
;;(prn (walk-and-insert {:step 3 :coll [0 5 2 4 3 6 1] :pos 5 :new-num 7}))

(defn part1 [step]
  (let [{:keys [coll pos]}
        (nth (iterate walk-and-insert
                      {:step step :coll [0] :pos 0 :new-num 1})
             2017)]
    (nth coll (inc pos))))

;; (prn (part1 3))
(prn :part-1 (part1 355))


;;; part 2

(defn walk-and-insert [{:keys [coll-size pos step new-num ansqwer]}]
  (let [keep-count (inc (rem (+ pos step) coll-size))]
    {:coll-size (inc coll-size)
     :pos keep-count
     :step step
     :new-num (inc new-num)
     :ansqwer (if (= keep-count 1)
                new-num
                ansqwer)}))

(defn part1 [step how-many]
  (nth (iterate walk-and-insert
                {:step step :coll-size 1 :pos 0 :new-num 1 :ansqwer :none})
       how-many))

;; (prn (part1 3))
(time (prn :part-2 (part1 355 50e6)))
