(ns chouser.day09
  (:require [clojure.string :as str]))

(defn puzzle-a [text]
  (reduce
   (fn [state char]
     (cond
       (:cancel? state) (assoc state :cancel? false)
       (= \! char) (assoc state :cancel? true)
       (:garbage? state) (assoc state :garbage? (not= \> char))
       (= \< char) (assoc state :garbage? true)
       (= \{ char) (-> state
                       (update :score + (:depth state))
                       (update :depth inc))
       (= \} char) (update state :depth dec)
       :else state))
   {:depth 1
    :score 0}
   text))

;;(prn 6 (puzzle-a "{{{}}}"))
;;(prn 9 (puzzle-a "{{<ab>},{<ab>},{<ab>},{<ab>}}"))
;;(prn 3 (puzzle-a "{{<a!>},{<a!>},{<a!>},{<ab>}}"))

;;(prn (puzzle-a (slurp "resources/input09.txt")))


(defn puzzle-b [text]
  (reduce
   (fn [state char]
     (cond
       (:cancel? state) (assoc state :cancel? false)
       (= \! char) (assoc state :cancel? true)
       (:garbage? state) (if (= \> char)
                           (assoc state :garbage? false)
                           (update state :garbage-count inc))
       (= \< char) (assoc state :garbage? true)
       (= \{ char) (-> state
                       (update :score + (:depth state))
                       (update :depth inc))
       (= \} char) (update state :depth dec)
       :else state))
   {:depth 1
    :score 0
    :garbage-count 0}
   text))

(prn (puzzle-b (slurp "resources/input09.txt")))
