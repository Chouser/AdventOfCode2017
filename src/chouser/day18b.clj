(ns chouser.day18b
  (:require [clojure.java.io :as io])
  (:import (clojure.lang ExceptionInfo IFn)))

(def input
  (for [line (line-seq (io/reader "resources/input18.txt"))]
    (read-string (str \[ line \]))))

(defn compile-body [prog rip]
  (->>
   prog
   (mapcat (fn [ip [op x y :as cmd]]
             (case op
               set `[(set! ~x ~y)]
               add `[(set! ~x (+ ~x ~y))]
               mul `[(set! ~x (* ~x ~y))]
               mod `[(set! ~x (rem ~x ~y))]
               snd `[(do (swap! ~'blk dec)
                         (deliver (first ~'cout) ~x)
                         (set! ~'send-count (inc ~'send-count))
                         (set! ~'cout (rest ~'cout)))]
               rcv `[(if (= 2 (swap! ~'blk inc))
                       (do
                         (deliver (first ~'cout) :done)
                         (recur -1))
                       (let [val# @(first ~'cin)]
                         (set! ~'cin (rest ~'cin))
                         (if (= :done val#)
                           (recur -1)
                           (do
                             (set! ~x (long val#))
                             (recur ~(inc ip))))))
                     :end]
               jgz `[(if (pos? ~x)
                       (recur ~(if (number? y) (+ ip y) `(+ ~ip ~y)))
                       (recur ~(inc ip)))
                     :end]))
           (drop rip (range)))
   (take-while #(not= % :end))
   (cons `do)))

(defn compile-fn [prog]
  (let [regs (map #(with-meta (symbol (str (char %))) {:tag 'long, :unsynchronized-mutable true})
                  (range (int \a) (int \q)))
        Prog (gensym "Prog-")
        pid (gensym "pid-")]
    `(do
       (deftype ~Prog [~(with-meta 'cin {:unsynchronized-mutable true})
                       ~(with-meta 'cout {:unsynchronized-mutable true})
                       ~(with-meta 'send-count {:unsynchronized-mutable true})
                       ~'blk
                       ~@regs]
         IFn
         (invoke [~'this]
           (loop [~'rip (long 0)]
             (case ~'rip
               -1 ~'send-count
               ~@(mapcat (fn [rip prog] [rip (compile-body prog rip)])
                         (range)
                         (take-while seq (iterate rest prog)))))))
       (fn run# [cin# cout# blk# ~pid]
         ((new ~(symbol (str (str *ns*) "." Prog))
                cin# cout# 0 blk#
                ~@(map #({'p pid} % 0) regs)))))))

(defn run-pair [prog]
  (let [a->b (repeatedly promise)
        b->a (repeatedly promise)
        f (eval (compile-fn prog))
        blk (atom 0)
        a (future (f b->a a->b blk 0))
        b (future (f a->b b->a blk 1))]
    @a
    (prn :part-2 @b)))

#_(binding [clojure.pprint/*print-right-margin* 150](clojure.pprint/pprint (compile-fn input)))
#_(run-pair '[[snd 1] [snd 2] [snd p] [rcv a] [rcv b] [rcv c] [rcv d]])
(run-pair input)
