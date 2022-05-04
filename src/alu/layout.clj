(ns alu.layout
  (:require [clojure.set :as set]
            [life.patterns :as patterns]
            [life.coords :as coords]))

(defn within-bounds?
  ([expression]
   (let [origin  (-> expression :alu/dimensions :alu/origin)
         width   (-> expression :alu/dimensions :alu/width)
         height  (-> expression :alu/dimensions :alu/height)
         output  (-> expression :alu/output :alu/position)
         pattern (expression :alu/pattern)]
     (within-bounds? pattern output origin width height)))
  ([pattern output [x0 y0] w h]
   (let [x1 (+ x0 w)
         y1 (+ y0 h)
         check (fn [[x y]] (and (>= x x0) (>= y y0) (< x x1) (< y y1)))]
     (and (check output)
          (every? identity (map check pattern))))))

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

(defn shift
  "Shifts an expression with the given offset."
  [expression offset]
  (let [origin (get-in expression [:alu/dimensions :alu/origin])
        position (get-in expression [:alu/output :alu/position])
        pattern (get expression :alu/pattern)]
    (-> expression
        (assoc-in [:alu/dimensions :alu/origin] (coords/add origin offset))
        (assoc-in [:alu/output :alu/position] (coords/add position offset))
        (assoc :alu/pattern (patterns/offset pattern offset)))))

(defn align-with-origin
  "Aligns the given expression so its origin is [0 0]."
  [expression]
  (let [[x y] (get-in expression [:alu/dimensions :alu/origin])
        offset [(- x) (- y)]]
    (shift expression offset)))

(defn- x-offset-at-origin [left right]
  (let [x1 (get-in left [:alu/dimensions :alu/origin 0])
        x2 (get-in right [:alu/dimensions :alu/origin 0])
        width (get-in left [:alu/dimensions :alu/width])]
    (- (+ x1 width 1) x2)))

(defn- x-offset-at-output [left right]
  (let [[x1] (get-in left [:alu/output :alu/position])
        [x2] (get-in right [:alu/output :alu/position])]
    (- x2 x1)))

(defn- y-offset-at-output [left right]
  (let [[_ y1] (get-in left [:alu/output :alu/position])
        [_ y2] (get-in right [:alu/output :alu/position])]
    (- y1 y2)))

(defn- x-offset-to-right-border [expression]
  (let [[x0] (-> expression :alu/dimensions :alu/origin)
        w    (-> expression :alu/dimensions :alu/width)
        [x1] (-> expression :alu/output :alu/position)]
    (- (+ x0 w) 2 x1)))

(defn- delay-expression [left right]
  (let [s1 (left :alu/steps)
        s2 (right :alu/steps)
        diff (int (/ (- s1 s2) 4))]
    (if (pos? diff)
      [left (wire right diff)]
      [(wire left (- diff)) right])))

(defn- calculate-offset [l r x-modifier-fn y-modifier-fn]
  (let [x-min     (x-offset-at-origin l r)
        x-diff    (x-offset-at-output l r)
        x         (x-modifier-fn x-min (x-offset-to-right-border l))
        x-offset  (if (odd? (+ x-min x-diff)) (inc x) x)
        y-offset  (y-modifier-fn (y-offset-at-output l r))]
    [x-offset y-offset]))

(defn change-direction [direction expression]
  (let [d (-> expression :alu/output :alu/direction)]
    (if (= d direction) expression (flip-x expression))))

(defn make-intersect
  "Aligns expressions by shifting one so that outputs will intersect.
  If both expression have a non zero output, they will cancel each other when intersecting.
  First output will face bottom-right and second output will face bottom-left."
  [left right]
  (let [l-flipped (change-direction :bottom-right left)
        r-flipped (change-direction :bottom-left right)
        [l r]     (delay-expression l-flipped r-flipped)
        offset    (calculate-offset l r (fn [x _] x) dec)]
    [l (shift r offset)]))

(defn make-parallel
  "Align expressions so they both face bottom right and outputs are synchronised."
  [left right]
  (let [l-flipped (change-direction :bottom-right left)
        r-flipped (change-direction :bottom-right right)
        [l r]     (delay-expression l-flipped r-flipped)
        offset    (calculate-offset l r (fn [x d] (+ x (* 2 d))) identity)]
    [l (shift r offset)]))

(defn merge-expressions
  "Merge two expressions into one, disregarding :output."
  [& expressions]
  {:pre [(apply = (map #(% :alu/steps) expressions))]}
  (let [xs      (map #(get-in % [:alu/dimensions :alu/origin 0]) expressions)
        ys      (map #(get-in % [:alu/dimensions :alu/origin 1]) expressions)
        x0      (apply min xs)
        y0      (apply min ys)
        width   (->> (map #(-> % :alu/dimensions :alu/width) expressions)
                     (map vector xs)
                     (map #(apply + %))
                     (map #(- % x0))
                     (apply max))
        height  (->> (map #(-> % :alu/dimensions :alu/height) expressions)
                     (map vector ys)
                     (map #(apply + %))
                     (map #(- % y0))
                     (apply max))
        steps   ((first expressions) :alu/steps)
        pattern (->> (map #(% :alu/pattern) expressions)
                     (reduce set/union))]
    {:alu/dimensions {:alu/origin [x0 y0]
                      :alu/width width
                      :alu/height height}
     :alu/steps steps
     :alu/pattern pattern}))

(defn spread-x
  "Spreads expressions on the X axis so they are adjacent and do not overlap."
  [expressions]
  (let [x0 (get-in (first expressions) [:alu/dimensions :alu/origin 0])
        old-origins (map #(get-in % [:alu/dimensions :alu/origin 0]) expressions)
        new-origins (->> (map #(get-in % [:alu/dimensions :alu/width]) expressions)
                         (reductions +)
                         drop-last
                         (cons 0)
                         (map (partial + x0)))
        x-offsets (map #(apply - %) (map vector new-origins old-origins))]
    (map (fn [[e x]] (shift e [x 0])) (map vector expressions x-offsets))))

(comment
  (within-bounds? #{[1 1] [1 2] [2 3] [3 4] [4 4]} [1 2] [1 2] 5 5)
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
