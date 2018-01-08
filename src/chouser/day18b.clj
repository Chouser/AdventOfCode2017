(ns chouser.day18b
  (:require [clojure.java.io :as io])
  (:import (clojure.lang ExceptionInfo IFn IDeref)))

(def input
  (for [line (line-seq (io/reader "resources/input18.txt"))]
    (read-string (str \[ line \]))))

(def ^:dynamic *trace* nil)

(defn compile-body [prog rip trace?]
  (->>
   prog
   (mapcat (fn [ip [op x y :as cmd]]
             (->
              (if (= 'jgz op)
                [`(if (pos? ~x)
                    (recur ~(if (number? y) (+ ip y) `(+ ~ip ~y)))
                    (recur ~(inc ip)))
                 :done]
                [(case op
                   set `(set! ~x ~y)
                   add `(set! ~x (+ ~x ~y))
                   mul `(set! ~x (* ~x ~y))
                   mod `(set! ~x (rem ~x ~y))
                   snd `(do (swap! ~'blk dec)
                            (deliver (first ~'cout) ~x)
                            (set! ~'cout (rest ~'cout)))
                   rcv `(if (= 2 (swap! ~'blk inc))
                          (do
                            (deliver (first ~'cout) :done)
                            (throw (ex-info "done" {:done 1})))
                          (let [val# @(first ~'cin)]
                            (set! ~'cin (rest ~'cin))
                            (if (= :done val#)
                              (throw (ex-info "done" {:done 2}))
                              (set! ~x (long val#))))))])
              ((fn [forms]
                 (if-not trace?
                   forms
                   (update forms 0
                           (fn [form]
                             `(do (swap! ~'*trace* conj {:ip ~ip :cmd '~cmd :state (deref ~'this) :recur-at ~rip})
                                  ~form))))))))
           (drop rip (range)))
   (take-while #(not= % :done))
   (cons `do)))

(defn compile-fn [prog & {:keys [trace?]}]
  (let [regs (map #(with-meta (symbol (str (char %))) {:tag 'long, :unsynchronized-mutable true})
                  (range (int \a) (int \q)))
        Prog (gensym "Prog-")
        pid (gensym "pid-")]
    `(do
       (deftype ~Prog [~(with-meta 'cin {:unsynchronized-mutable true})
                       ~(with-meta 'cout {:unsynchronized-mutable true})
                       ~'blk
                       ~@regs]
         IDeref
         (deref [~'this] (into {} (map (fn [k# v#] (when-not (zero? v#) [k# v#]))
                                       '~regs [~@(map #(with-meta % nil) regs)])))
         IFn
         (invoke [~'this]
           (loop [~'rip (long 0)]
             (case ~'rip
               ~@(mapcat (fn [rip prog] [rip (compile-body prog rip trace?)])
                         (range)
                         (take-while seq (iterate rest prog)))))))
       (fn run# [cin# cout# blk# ~pid]
         ((new ~(symbol (str (str *ns*) "." Prog))
                cin# cout# blk#
                ~@(map #({'p pid} % 0) regs)))))))

(defn run-pair [prog]
  (let [a->b (repeatedly promise)
        b->a (repeatedly promise)
        f (eval (compile-fn prog :trace? true))
        blk (atom 0)
        trace-a (atom [])
        trace-b (atom [])
        a (future (binding [*trace* trace-a] (f b->a a->b blk 0)))
        b (future (binding [*trace* trace-b] (f a->b b->a blk 1)))]
    (prn :a-done (try @a (catch Exception e e)))
    #_(def ta trace-a)
    #_(def tb trace-b)
    (prn :b-done (try @b (catch Exception e e)))
    @trace-a
    (prn :part-2 (count (take-while true? (map realized? (iterate rest b->a)))))))

#_(binding [clojure.pprint/*print-right-margin* 150](clojure.pprint/pprint (compile-fn input)))
#_(run-pair '[[snd 1] [snd 2] [snd p] [rcv a] [rcv b] [rcv c] [rcv d]])
#_(run-pair input)
