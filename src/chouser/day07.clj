(ns chouser.day07
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def raw
  (for [line (line-seq (io/reader "resources/input07.txt"))]
    (let [[_ base x _ kids-str] (re-matches #"(\w+) \((\d+)\)( -> (.*))?" line)
          kids (when kids-str (str/split kids-str #", "))]
      {:base base :x (Integer/parseInt x) :kids kids})))

(def base-map
  (reduce (fn [base-map [base kid]] (assoc base-map kid base))
          {}
          (mapcat (fn [{:keys [base kids]}]
                    (map vector (repeat base) kids))
                  raw)))

(def root
  (last
   (take-while identity
               (iterate base-map (val (first base-map))))))

(prn :root root)


(def kid-map (zipmap (map :base raw) raw))

(defn weight [name]
  (let [{:keys [x kids]} (get kid-map name)
        kid-weights (map weight kids)
        answer (first (filter map? kid-weights))]
    (if answer
      answer
      (if (and (seq kids) (not (apply = kid-weights)))
        (let [freqs (frequencies kid-weights)
              bad-total (get (zipmap (vals freqs) (keys freqs)) 1)
              good-total (key (first (dissoc freqs bad-total)))
              delta (- good-total bad-total)
              bad-kid (get (zipmap kid-weights kids) bad-total)]
          {:fixed-weight (+ delta (get-in kid-map [bad-kid :x]))})
        (reduce + x kid-weights)))))

(prn (weight root))
