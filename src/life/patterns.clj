(ns life.patterns
  (:require [life.coords :as coords]))

(def glider
  "-o-
   --o
   ooo"
   #{[1 0] [2 1] [0 2] [1 2] [2 2]})

(def eater
   "---oo--oo
    o---o--oo
    oooo-----
    ---------
    oo-------
    oo-------"
   #{[3 0] [4 0] [7 0] [8 0]
     [0 1] [4 1] [7 1] [8 1]
     [0 2] [1 2] [2 2] [3 2]
     [0 4] [1 4] [0 5] [1 5]})

(def block
   "oo
    oo"
   #{[0 0] [1 0] [0 1] [1 1]})

(defn offset
  "Offset a pattern with the given coordinate."
  [pattern coordinate]
  (set (map (partial coords/add coordinate) pattern)))

(defn flip-x
  "Flip a pattern on the X axis."
  ([pattern]
   (let [x-max (apply max (map first pattern))]
     (flip-x pattern 0 (inc x-max))))
  ([pattern x0 w]
   (set (map #(coords/flip-x % x0 w) pattern))))

(comment
  (offset glider [1 2]))
