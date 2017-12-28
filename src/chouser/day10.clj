(ns chouser.day10)

(def lengths [189,1,111,246,254,2,0,120,215,93,255,50,84,15,94,62])

(defn twist [{:keys [string pos skip] :as state} length]
  {:string (let [string+ (cycle string)
                 ;; Compute the twist:
                 string+ (concat (take pos string+)
                                 (reverse (take length (drop pos string+)))
                                 (drop (+ pos length) string+))]
             ;; Modulo string length:
             (take (count string)
                   (concat (take pos (drop (count string) string+))
                           (drop pos string+))))
   :pos (rem (+ pos length skip) (count string))
   :skip (rem (inc skip) (count string))})

;; (-> {:string (range 5) :pos 0 :skip 0} (twist 3) (twist 4))

(let [{[a b] :string} (reduce twist {:string (range 256) :pos 0 :skip 0} lengths)]
  (prn :puzzle-a (* a b)))


(def input "189,1,111,246,254,2,0,120,215,93,255,50,84,15,94,62")

(defn puzzle-b [input-str]
  (->>
   (concat (map int input-str) [17, 31, 73, 47, 23])
   (repeat 64)
   (mapcat seq)
   (reduce twist {:string (range 256) :pos 0 :skip 0})
   :string
   (partition 16)
   (map #(format "%02x" (reduce bit-xor %)))
   (apply str)))

(prn :empty (puzzle-b ""))
(prn "1,2,3" (puzzle-b "1,2,3"))
(prn :puzzle-b (puzzle-b input))
