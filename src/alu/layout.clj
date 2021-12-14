(ns alu.layout
  (:require [clojure.set :as set]
            [life.patterns :as patterns]
            [life.coords :as coords]))

(defn within-bounds?
  ([expression]
   (let [origin (get-in expression [:alu/dimensions :alu/origin])
         width (get-in expression [:alu/dimensions :alu/width])
         height (get-in expression [:alu/dimensions :alu/height])
         pattern (expression :alu/pattern)]
     (within-bounds? pattern origin width height)))
  ([pattern [x0 y0] w h]
   (let [x1 (+ x0 w)
         y1 (+ y0 h)]
     (every? identity (map
                        (fn [[x y]] (and (>= x x0) (>= y y0) (< x x1) (< y y1)))
                        pattern)))))

(defn flip-x
  "Flip exprssion on the X axis."
  [expression]
  (let [{:keys [alu/pattern]
         {:keys [alu/origin alu/width]} :alu/dimensions
         {:keys [alu/position alu/direction]} :alu/output} expression
        [x0] origin
        new-direction (if (= direction :bottom-right) :bottom-left :bottom-right)
        new-position (coords/flip-x position x0 width)
        new-pattern (patterns/flip-x pattern x0 width)]
    (-> expression
        (assoc-in [:alu/output :alu/direction] new-direction)
        (assoc-in [:alu/output :alu/position] new-position)
        (assoc :alu/pattern new-pattern))))

(defn wire
  "Allow transmission of one bit over a distance."
  [expression distance]
  (let [{:keys [alu/steps]
         {:keys [alu/origin alu/width alu/height]} :alu/dimensions
         {:keys [alu/position alu/direction]} :alu/output} expression
        [x0 y0] origin
        [x y] (coords/add position [distance distance])]
    (if (= direction :bottom-left)
      (flip-x (wire (flip-x expression) distance))
      (-> expression
          (assoc-in [:alu/dimensions :alu/width] (max width (+ 2 (- x x0))))
          (assoc-in [:alu/dimensions :alu/height] (max height (+ 2 (- y y0))))
          (assoc-in [:alu/output :alu/position] [x y])
          (assoc :alu/steps (+ steps (* 4 distance)))))))

(defn- shift [expression offset]
  (let [origin (get-in expression [:alu/dimensions :alu/origin])
        position (get-in expression [:alu/output :alu/position])
        pattern (get expression :alu/pattern)]
    (-> expression
        (assoc-in [:alu/dimensions :alu/origin] (coords/add origin offset))
        (assoc-in [:alu/output :alu/position] (coords/add position offset))
        (assoc :alu/pattern (patterns/offset pattern offset)))))

(defn- x-offset-at-origin [left right]
  (let [x1 (get-in left [:alu/dimensions :alu/origin 0])
        x2 (get-in right [:alu/dimensions :alu/origin 0])
        width (get-in left [:alu/dimensions :alu/width])]
    (- (+ x1 width) x2)))

(defn- x-offset-at-output [left right]
  (let [[x1] (get-in left [:alu/output :alu/position])
        [x2] (get-in right [:alu/output :alu/position])]
    (- x2 x1)))

(defn- y-offset-at-output [left right]
  (let [[_ y1] (get-in left [:alu/output :alu/position])
        [_ y2] (get-in right [:alu/output :alu/position])]
    (dec (- y1 y2))))

(defn- delay-expression [left right]
  (let [s1 (left :alu/steps)
        s2 (right :alu/steps)
        diff (int (/ (- s1 s2) 4))]
    (if (pos? diff)
      [left (wire right diff)]
      [(wire left (- diff)) right])))

(defn align-for-intersection
  "Aligns expressions by shifting one so that outputs will intersect.
  If both expression have a non zero output, they will cancel each other when intersecting.
  First output will face bottom-right and second output will face bottom-left."
  [left right]
  {:pre [(not= (get-in left [:alu/output :alu/direction]) (get-in right [:alu/output :alu/direction]))]}
  (if (= :bottom-left (get-in left [:alu/output :alu/direction]))
    (align-for-intersection right left)
    (let [[l r] (delay-expression left right)
          x-min (x-offset-at-origin l r)
          x-diff (x-offset-at-output l r)
          x-offset (if (odd? (+ x-min x-diff)) (inc x-min) x-min)
          y-offset (y-offset-at-output l r)]
      [l (shift r [x-offset y-offset])])))

(defn merge-expressions
  "Merge two expressions into one, disregarding :output and :step."
  [left right]
  (let [[x0 y0] (get-in left [:alu/dimensions :alu/origin])
        [x1 y1] (get-in right [:alu/dimensions :alu/origin])
        w0 (get-in left [:alu/dimensions :alu/width])
        h0 (get-in left [:alu/dimensions :alu/height])
        w1 (get-in right [:alu/dimensions :alu/width])
        h1 (get-in right [:alu/dimensions :alu/height])
        x-min (min x0 x1)
        y-min (min y0 y1)
        x-max (max (+ x0 w0) (+ x1 w1))
        y-max (max (+ y0 h0) (+ y1 h1))]
    {:alu/dimensions {:alu/origin [x-min y-min]
                      :alu/width (- x-max x-min)
                      :alu/height (- y-max y-min)}
     :alu/pattern (set/union (left :alu/pattern) (right :alu/pattern))}))

(defn spread-x
  "Spreads expressions on the X axis so they are adjacent and do not overlap."
  [expressions]
  (let [origin (get-in (expressions 0) [:alu/dimensions :alu/origin 0])]
    (->> (map #(get-in % [:alu/dimensions :alu/width]) expressions)
         (reductions +)
         drop-last
         (cons 0)
         (map (partial + origin))
         (map vector expressions)
         (map (fn [[e x]] (assoc-in e [:alu/dimensions :alu/origin 0] x))))))

(comment
  (within-bounds? #{[1 1] [1 2] [2 3] [3 4] [4 4]} [1 2] 5 5)
  (let [init {:alu/dimensions {:alu/origin [0 0]
                               :alu/width 5
                               :alu/height 5}
              :alu/output {:alu/position [3 3]
                           :alu/direction :bottom-right}
              :alu/steps 0
              :alu/pattern (patterns/offset patterns/glider [1 1])}
        shifted (shift init [2 1])]
    (println init)
    (println (shift init [2 1]))
    (println (x-offset-at-origin shifted init))
    (println (x-offset-at-output init shifted))
    (println (y-offset-at-output init shifted))))
