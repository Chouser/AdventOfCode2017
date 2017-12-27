(ns chouser.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn puzzle-a [mem]
  (loop [mem mem, ip 0, step 0]
    (if (< -1 ip (count mem))
      (recur (update mem ip inc)
             (+ ip (get mem ip))
             (inc step))
      step)))

(prn :test-a (puzzle-a [0 3 0 1 -3]))
(prn :actual-a (puzzle-a (mapv #(Integer/parseInt %)
                               (line-seq (io/reader "resources/input05.txt")))))


(defn puzzle-b [mem]
  (loop [mem mem, ip 0, step 0]
    (if (< -1 ip (count mem))
      (recur (update mem ip #(+ % (if (<= 3 %) -1 1)))
             (+ ip (get mem ip))
             (inc step))
      step)))

(prn :test-b (puzzle-b [0 3 0 1 -3]))
(prn :actual-b (puzzle-b (mapv #(Integer/parseInt %)
                               (line-seq (io/reader "resources/input05.txt")))))
