(ns chouser.day11
  (:require [clojure.string :as str]))

(def input (str/split (slurp "resources/input11.txt") #","))

(defn step [[x y] [v h]]
  [(+ x ({\w -1, \e 1} h 0))
   (+ y (cond
          (nil? h)  ({\n -1} v 1)
          (even? x) ({\n -1} v 0)
          (odd? x)  ({\n  0} v 1)))])

(defn origin-distance [[x y]]
  (+ (Math/abs x)
     (Math/abs y)
     (quot (Math/abs x) -2)))

(prn :puzzle-a (origin-distance (reduce step [0 0] input)))

(prn :puzzle-b (apply max (map origin-distance (reductions step [0 0] input))))
