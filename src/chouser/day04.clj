(ns chouser.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; 325
(prn
 (reduce +
         (for [line (line-seq (io/reader "resources/input04.txt"))]
           (let [words (str/split line #"\s+")]
             (if (= (count words) (count (distinct words)))
               1
               0)))))

;; 119
(prn
 (reduce +
         (for [line (line-seq (io/reader "resources/input04.txt"))]
           (let [words (map sort (str/split line #"\s+"))]
             (if (= (count words) (count (distinct words)))
               1
               0)))))
