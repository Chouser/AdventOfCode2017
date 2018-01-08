(ns chouser.day18
  (:require [clojure.java.io :as io]))

(def input
  (vec
   (for [line (line-seq (io/reader "resources/input18.txt"))]
     (read-string (str \[ line \])))))

(defn step [{:keys [prog played ip recover] :as s}]
  (let [[op x y] (get prog ip)
        v (fn [x]
            (if (symbol? x)
              (get s x 0)
              x))
        s (update s :ip inc)]
    (case op
      snd (assoc s :played (v x))
      set (assoc s x (v y))
      add (update s x (fnil + 0) (v y))
      mul (update s x (fnil * 0) (v y))
      mod (update s x (fnil rem 0)(v y))
      rcv (if (zero? (v x)) s (recover s))
      jgz (if (pos? (v x))
            (update s :ip + (dec (v y)))
            s))))

;; (prn (dissoc (step {:prog input :ip 0}) :prog))

#_
(prn
 (loop [s {:prog input :ip 0 :recover #(assoc % :a (:played %))}]
   (or (:a s) (recur (step s)))))

;; part 2

(defn step [{:keys [prog ip inbox] :as s}]
  (let [[op x y :as cmd] (nth prog ip)
        v (fn [x]
            (if (symbol? x)
              (get s x 0)
              x))
        s (-> s
              (update :ip inc)
              (assoc :blocked false))]
    (case op
      snd (-> s
              (update :outbox conj (v x))
              (update :send-count inc))
      set (assoc s x (v y))
      add (update s x (fnil +   0) (v y))
      mul (update s x (fnil *   0) (v y))
      mod (update s x (fnil rem 0) (v y))
      rcv (if (seq inbox)
            (-> s
                (assoc x (peek inbox))
                (update :inbox pop))
            (-> s
                (assoc :blocked true)
                (update :ip dec)))
      jgz (if (pos? (v x))
            (update s :ip + (dec (v y)))
            s))))

(def init {:ip 0
           :send-count 0
           :outbox []
           :inbox clojure.lang.PersistentQueue/EMPTY})

(defn move-msgs [from to ss]
  (-> ss
      (update-in [to :inbox] into (get-in ss [from :outbox]))
      (update-in [from :outbox] empty)))

(defn run-pair [prog]
  (loop [ss [(assoc init 'p 0 :prog prog)
             (assoc init 'p 1 :prog prog)]]
    (if (every? #(and (:blocked %) (empty? (:outbox %))) ss)
      (prn :part-2 (get-in ss [1 :send-count]))
      (recur (->> ss
                  (move-msgs 0 1)
                  (move-msgs 1 0)
                  (mapv step))))))

;; 127 is too low (was running each thread only to the first blocking state)

(run-pair '[[snd 1] [snd 2] [snd p] [rcv a] [rcv b] [rcv c] [rcv d]])
#_(run-pair input)
