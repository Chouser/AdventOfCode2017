(ns chouser.day14
  (:require [chouser.day10 :as day10]))

(def input "jzgqcdpd")

(defn knot-hash [input-str]
  (->>
   (concat (map int input-str) [17, 31, 73, 47, 23])
   (repeat 64)
   (mapcat seq)
   (reduce day10/twist {:string (range 256) :pos 0 :skip 0})
   :string
   (partition 16)
   (map #(reduce bit-xor %))))

(defn count-bits [input-str]
  (reduce +
          (for [x (range 128)]
            (count (filter #{\1} (mapcat #(Integer/toString % 2) (knot-hash (str input-str "-" x))))))))

;;(prn :part-1 (count-bits input))

;; part 2

(defonce grid
  (mapv (fn [x]
          (vec (mapcat #(format "%8s" (Integer/toString % 2)) (knot-hash (str input "-" x)))))
        (range 128)))

;; 6 is wrong
;; 1098 is too low
;; 1112 is wrong
;; 1025 is wrong
;; 1212 is right! But why?
(defn fill [grid]
  (reduce (fn [{:keys [groups grid id] :as state} coord]
            (if (not= \1 (get-in grid coord))
              state
              (let [[nbr1 nbr2] (filter int? (map #(get-in grid (map + coord %))
                                                  [[0 -1] [-1 0]]))]
                {:groups (cond
                           nbr2 (disj groups nbr2)
                           nbr1 groups
                           :else (conj groups id))
                 :grid ;;(assoc-in grid coord (or nbr1 id))
                 (if nbr2
                   (mapv (fn [row] (mapv (fn [c] (if (= c nbr2) nbr1 c)) row))
                         (assoc-in grid coord nbr1))
                   (assoc-in grid coord (or nbr1 id)))
                 :id (if nbr1 id (inc id))})))
          {:groups #{} :grid grid :id 0}
          (for [y (range 128), x (range 128)] [y x])))

(defn fill [grid]
  (reduce (fn [{:keys [groups grid id] :as state} coord]
            (if (not= \1 (get-in grid coord))
              state
              (let [[nbr1 nbr2] (distinct
                                 (filter int? (map #(get-in grid (map + coord %))
                                                   [[0 -1] [-1 0]])))]
                {:groups (cond
                           nbr2 (disj groups nbr2)
                           nbr1 groups
                           :else (conj groups id))
                 :grid #_(assoc-in grid coord (or nbr1 id))
                 (if nbr2
                     (mapv (fn [row] (mapv (fn [c] (if (= c nbr2) nbr1 c)) row))
                           (assoc-in grid coord nbr1))
                     (assoc-in grid coord (or nbr1 id)))
                 :id (if nbr1 id (inc id))})))
          {:groups #{} :grid grid :id 0}
          (for [y (range 128), x (range 128)] [y x])))

(defonce good-fill (fill grid))
(defonce bad-fill (fill grid))
(defonce bad-groups (reduce disj (:groups bad-fill) (:groups good-fill)))

(defn print-fill [fill-state]
  (let [{:keys [groups grid id]} fill-state]
    (doseq [row grid]
      (println (apply str (map #(format "%2s" (if (int? %) (str (when (contains? bad-groups %) \*) (char (+ (int \! )(rem % 90)))) "")) row))))
    #_(prn :groups (sort groups) :id id)
    #_(prn :gridps (sort (set (filter int? (mapcat seq grid)))))
    (prn :count (count groups))
    #_(prn :count-again (count (distinct (filter int? (mapcat seq grid)))))
    ))

(defonce test-grid
  (mapv (fn [x]
          (vec (mapcat #(format "%8s" (Integer/toString % 2)) (knot-hash (str "flqrgnkx-" x)))))
        (range 128)))

(print-fill (fill grid))

