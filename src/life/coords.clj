(ns life.coords)

(defn add
  "Adds two coordinates."
  [left right]
  (vector (+ (first left) (first right)) (+ (second left) (second right))))

(defn neighbours
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]))
