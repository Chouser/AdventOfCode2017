(ns chouser.day08
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (for [line (line-seq (io/reader "resources/input08.txt"))]
    (-> (zipmap
         [:line :setreg :dir :delta :testreg :testop :testval]
         (re-matches #"(\w+) (inc|dec) ([-0-9]+) if (\w+) (\S+) ([-0-9]+)" line))
        (update :delta #(Integer/parseInt %))
        (update :testval #(Integer/parseInt %)))))

(->> input
     (reduce (fn [mem {:keys [setreg dir delta testreg testop testval]}]
               (let [op (or (resolve (symbol testop)) not=)]
                 (if (op (get mem testreg 0) testval)
                   (update mem setreg (fnil + 0) (* delta (if (= dir "dec") -1 1)))
                   mem)))
             {})
     vals
     (apply max)
     prn)


;; part 2:

(->> input
     (reduce (fn [mem {:keys [setreg dir delta testreg testop testval]}]
               (let [op (or (resolve (symbol testop)) not=)]
                 (if (op (get mem testreg 0) testval)
                   (let [new-val (+ (get mem setreg 0) (* delta (if (= dir "dec") -1 1)))]
                     (-> mem
                         (assoc setreg new-val)
                         (update :maxval max new-val)))
                   mem)))
             {:maxval 0})
     :maxval
     prn)
