(ns life.coords
  (:require [clojure.spec.alpha :as s]))

(s/def :coords/coords (s/tuple int? int?))

(defn add
  "Adds two coordinates."
  [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])

(defn sub
  "Substract two coordinates."
  [[x0 y0] [x1 y1]]
  [(- x0 x1) (- y0 y1)])

(defn neighbours
  "Returns the 8 neighbours of a given coordinate."
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]))

(defn flip-x
  "Flip a coord on the X axis, bounded by an origin and a width."
  [[x y] x0 w]
  [(dec (+ x0 (- w (- x x0)))) y])

(comment
  (s/explain :coords/coords [1 2]))
