(ns life.coords)

(defn add
  "Adds two coordinates."
  [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])

(defn sub
  "Substract two coordinates."
  [[x0 y0] [x1 y1]]
  [(- x0 x1) (- y0 y1)])

(defn neighbours
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]))
