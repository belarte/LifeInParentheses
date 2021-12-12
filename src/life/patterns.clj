(ns life.patterns
  (:require [clojure.spec.alpha :as s]
            [life.coords :as coords]))

(s/def :patterns/pattern (s/coll-of :coords/coords :kind set? :distinct true))

(def glider
  "-o-
   --o
   ooo"
   #{[1 0] [2 1] [0 2] [1 2] [2 2]})

(def eater
   "oo--
    o---
    -ooo
    ---o"
   #{[0 0] [1 0] [0 1] [1 2] [2 2] [3 2] [3 3]})

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
  (s/explain :patterns/pattern #{[1 2] [1 3]})
  (offset glider [1 2]))
