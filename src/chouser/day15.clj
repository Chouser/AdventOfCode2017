(ns chouser.day15)

;; Generator A starts with 679
;; Generator B starts with 771

(defn generator [^long factor]
  (fn ^long [^long prev]
    (rem (* factor prev) 2147483647)))

(def gen-a (generator 16807))
(def gen-b (generator 48271))

(defn judgeable-seq [coll]
  (map #(bit-and 0xffff ^long %) coll))

(defn judge [init-a init-b limit]
  (->>
   (map ==
        (take (inc limit) (judgable-seq (iterate gen-a init-a)))
        (take (inc limit) (judgable-seq (iterate gen-b init-b))))
   (filter true?)
   count))

;; (prn :test=1   (judge 65 8921 5))
;; (prn :test=588 (time (judge 65 8921 40e6))) ;; 27748 msecs
;; (prn :part-1 (time (judge 679 771 40e6)))

;; hmph. should be faster.

(def judgeable-xform
  (map #(bit-and 0xffff ^long %)))

(defn judgeable-vec [gen init limit]
  (into (vector-of :long)
        (comp (take limit)
              judgeable-xform)
        (iterate gen init)))

(defn judge [init-a init-b limit]
  (->>
   (map ==
        (judgeable-vec gen-a init-a limit)
        (judgeable-vec gen-b init-b limit))
   (filter true?)
   count))

;;(prn :test=1   (judge 65 8921 5))
;;(prn :test=588 (time (judge 65 8921 40e6))) ;; 48366! I made it worse

;; == part 2

(defn judge2 [init-a init-b limit]
  (->>
   (map ==
        (take (inc limit) (judgable-seq (filter #(zero? (rem % 4)) (iterate gen-a init-a))))
        (take (inc limit) (judgable-seq (filter #(zero? (rem % 8)) (iterate gen-b init-b)))))
   (filter true?)
   count))

;; 2377 is too high; 161176 msecs.  Oops, that was still 40 million.
;; (prn :part-2 (time (judge2 679 771 5e6))) ;; 306. 14359 msecs

;; another optimization attempt:

(defprotocol LongIterator
  (next-long ^long [this] "Returns the next long in the sequence"))

(deftype Gen [^long factor, ^long mult, ^:unsynchronized-mutable ^long value]
  LongIterator
  (next-long [_]
    (loop []
      (set! value (rem (* factor value) 2147483647))
      (if (zero? (rem value mult))
        value
        (recur)))))

(defn judge3 [[mult-a init-a] [mult-b init-b] limit]
  (let [mgen-a (Gen. 16807 mult-a init-a)
        mgen-b (Gen. 48271 mult-b init-b)
        match-count (atom 0)]
    (dotimes [_ limit]
      (when (= (bit-and 0xffff (.next-long mgen-a))
               (bit-and 0xffff (.next-long mgen-b)))
        (swap! match-count inc)))
    @match-count))

;; (prn :test=1 (judge3 [1 65] [1 8921] 5))
;; (prn :test=588 (time (judge3 [1 65] [1 8921] 40e6))) ;; 3359 msecs. Oh yeah.
;; (prn :part-1 (time (judge3 [1 679] [1 771] 40e6))) ;; 626 in 3.4 seconds
;; (prn :part-2-too-long (time (judge3 [4 679] [8 771] 40e6))) ;; 2377 in 16.5 seconds
(prn :part-2 (time (judge3 [4 679] [8 771] 5e6))) ;; 306 in 2.1 seconds
