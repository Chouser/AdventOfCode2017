(ns chouser.day16
  (:require [clojure.string :as str]))

;; oejcphdbaifkgnlm is wrong ('s' counted from the wrong end)
;; pjakmbncdhglifeo is wrong ('p' was accidental no-op)
;; ionlbkfeajgdmphc is right
#_
(prn :part-1
     (apply str
            (reduce
             (fn [state [_ op a b]]
               #_(prn state op a b)
               (case op
                 "s" (let [[as bs] (split-at (- (count state) (Integer/parseInt a))
                                             state)]
                       (concat bs as))
                 "x" (let [a (Integer/parseInt a)
                           b (Integer/parseInt b)]
                       (-> (vec state)
                           (assoc b (nth state a))
                           (assoc a (nth state b))))
                 "p" (map #({a b, b a} % %) (map str state))))
             "abcdefghijklmnop"
             (re-seq #"([sxp])(\w+)(?:/(\w+))?"
                     (slurp "resources/input16.txt")))))

;; takes 20 seconds to do this 100 times. Not gonna work for part 2...

;; == part 2

(def prog
  (mapv (fn [[_ op a b]]
          (case op
            "s" [:s (Integer/parseInt a)]
            "x" [:x (Integer/parseInt a) (Integer/parseInt b)]
            "p" [:p (first a) (first b)]))
        (re-seq #"([sxp])(\w+)(?:/(\w+))?"
                (slurp "resources/input16.txt"))))

(defn run-prog
  ([c] (run-prog prog c))
  ([proc c]
   (reduce
    (fn [c [op a b]]
      (case op
        :s (let [i (- (count c) a)]
             (into (subvec c i) (subvec c 0 i)))
        :x (-> c
               (assoc b (get c a))
               (assoc a (get c b)))
        :p (mapv #({a b, b a} % %) c)))
    c
    prog)))

#_(prn :take2 (time (apply str (nth (iterate run-prog (vec "abcdefghijklmnop"))
                                  100))))

;; takes 1 second for that 100 iterations. Still no good...

;; Nathan suggests taking the mapping for a whole dance and treating it as a single step

(def init (vec "abcdefghijklmnop"))

(defn make-dance-map [f]
  (map #(- (int %) (int \a))
       (f (vec "abcdefghijklmnop"))))

(defn apply-dance-times [dance-map times state]
  (nth (iterate #(mapv % dance-map) state) times))

#_(let [one-dance-map (make-dance-map run-prog)
      million-dance-map (make-dance-map #(apply-dance-times one-dance-map 1e6 %))]

  (doseq [c (take 20 (iterate #(apply-dance-times one-dance-map 1 %) init))]
    (prn (apply str c)))

  #_(prn :part-2 (apply str (apply-dance-times million-dance-map 1e3 init))))

;; abndekfhijglmpoc is wrong
;; abpdegkhijflmcon is also wrong

#_(println "-- run-prog")
#_(doseq [c (take 20 (iterate run-prog init))]
  (prn (apply str c)))

;; ARGH! The idea of a "dance map" doesn't work with p commands

;; abc (pb/c) acb (s2) cba
;; abc (s2)   bca (pb/c) cba

;; Nathan says we should try putting all the p's together to optimize the rest
;; And it seems to work

#_(let [prog-p-at-end (vec (concat
                          (remove #(= :p (first %)) prog)
                          (filter #(= :p (first %)) prog)))]
  (doseq [c (take 20 (iterate #(run-prog prog-p-at-end %) init))]
    (prn (apply str c))))

(defn dance-twice [c]
  (let [rog (vec (remove #(= :p (first %)) prog))]
    (->> c
         (run-prog rog)
         (run-prog rog))))

#_(println "-- dance-twice * 10")
#_(doseq [c (take 10 (iterate #(dance-twice %) init))]
  (prn (apply str c)))

#_(let [states (take 10 (iterate #(dance-twice %) init))]
  (doseq [[a b] (map vector states (next states))]
    (prn (apply str a)
         (map #(some identity (map-indexed (fn [i t] (if (= t %) i)) a)) b))))

#_(let [two-dance-map (make-dance-map dance-twice)
      million-dance-map (make-dance-map #(apply-dance-times two-dance-map 500e3 %))]

  (println "-- two-dance-map * 10")
  (doseq [c (take 10 (iterate #(apply-dance-times two-dance-map 1 %) init))]
    (prn (apply str c)))

  #_(prn :part-2 (apply str (apply-dance-times million-dance-map 1e3 init))))

;; jfbihedaglmpokcn is also wrong!!!
;; dobpnfkemjgialhc is STILL wrong

;; Looking for a looping pattern!!
(println "-- pattern at 24?")
(doseq [c (take 48 (iterate run-prog init))]
  (prn (apply str c)))

;; fdnphiegakolcmjb  -- WHAT!!!!  Saw a loop at 30, (rem 1e9 30)
