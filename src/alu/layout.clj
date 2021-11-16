(ns alu.layout
  (:require [life.patterns :as patterns]
            [life.coords :as coords]))

(defn flip-x
  "Flip exprssion on the X axis."
  [expression]
  (let [{:keys [pattern]
         {:keys [origin width]} :dimensions
         {:keys [position direction]} :output} expression
        [x0] origin
        new-direction (if (= direction :bottom-right) :bottom-left :bottom-right)
        new-position (coords/flip-x position x0 width)
        new-pattern (patterns/flip-x pattern x0 width)]
    (-> expression
        (assoc-in [:output :direction] new-direction)
        (assoc-in [:output :position] new-position)
        (assoc :pattern new-pattern))))

(defn wire
  "Allow transmission of one bit over a distance."
  [expression distance]
  (let [{:keys [steps]
         {:keys [origin width height]} :dimensions
         {:keys [position direction]} :output} expression
        [x0 y0] origin
        [x y] (coords/add position [distance distance])]
    (if (= direction :bottom-left)
      (flip-x (wire (flip-x expression) distance))
      (-> expression
          (assoc-in [:dimension :width] (max width (+ 2 (- x x0))))
          (assoc-in [:dimension :height] (max height (+ 2 (- y y0))))
          (assoc-in [:output :position] [x y])
          (assoc :steps (+ steps (* 4 distance)))))))

(defn- shift [expression offset]
  (let [origin (get-in expression [:dimensions :origin])
        position (get-in expression [:output :position])
        pattern (get expression :pattern)]
    (-> expression
        (assoc-in [:dimensions :origin] (coords/add origin offset))
        (assoc-in [:output :position] (coords/add position offset))
        (assoc :pattern (patterns/offset pattern offset)))))

(defn- x-offset-at-origin [left right]
  (let [x1 (get-in left [:dimensions :origin 0])
        x2 (get-in right [:dimensions :origin 0])
        width (get-in left [:dimensions :width])]
    (- (+ x1 width) x2)))

(defn- x-offset-at-output [left right]
  (let [[x1] (get-in left [:output :position])
        [x2] (get-in right [:output :position])]
    (- x2 x1)))

(defn- y-offset-at-output [left right]
  (let [[_ y1] (get-in left [:output :position])
        [_ y2] (get-in right [:output :position])]
    (dec (- y1 y2))))

(defn- delay-expression [left right]
  (let [s1 (left :steps)
        s2 (right :steps)
        diff (int (/ (- s1 s2) 4))]
    (if (pos? diff)
      [left (wire right diff)]
      [(wire left (- diff)) right])))

(defn align-for-intersection
  "Aligns expressions by shifting one so that outputs will intersect.
  If both expression have a non zero output, they will cancel each other when intersecting.
  First output will face bottom-right and second output will face bottom-left."
  [left right]
  {:pre [(not= ((left :output) :direction) ((right :output) :direction))]}
  (if (= :bottom-left (get-in left [:output :direction]))
    (align-for-intersection right left)
    (let [[l r] (delay-expression left right)
          x-min (x-offset-at-origin l r)
          x-diff (x-offset-at-output l r)
          x-offset (if (even? (+ x-min x-diff)) (inc x-min) x-min)
          y-offset (y-offset-at-output l r)]
      [l (shift r [x-offset y-offset])])))

(comment
  (let [init {:dimensions {:origin [0 0]
                           :width 5
                           :height 5}
              :output {:position [3 3]
                       :direction :bottom-right}
              :steps 0
              :pattern (patterns/offset patterns/glider [1 1])}
        shifted (shift init [2 1])]
    (println init)
    (println (shift init [2 1]))
    (println (x-offset-at-origin shifted init))
    (println (x-offset-at-output init shifted))
    (println (y-offset-at-output init shifted))))
