(ns alu.layout
  (:require [clojure.set :as set]
            [life.patterns :as patterns]
            [life.coords :as coords]))

(defn => [expression args]
  (let [generator  (expression :alu/generator)]
    (-> expression
        (assoc :alu/pattern (generator args))
        (dissoc :alu/generator))))

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

(defn flip-x>
  "Generator for flipping expression on the X axis."
  [expression]
  (let [[x0 _]     (-> expression :alu/dimensions :alu/origin)
        width      (-> expression :alu/dimensions :alu/width)
        dir        (-> expression :alu/output :alu/direction)
        position   (-> expression :alu/output :alu/position)
        gen        (-> expression :alu/generator)
        direction  (if (= dir :bottom-right) :bottom-left :bottom-right)
        output     (coords/flip-x position x0 width)
        generator  (fn [args]
                     (patterns/flip-x (gen args) x0 width))]
    (-> expression
        (assoc-in [:alu/output :alu/direction] direction)
        (assoc-in [:alu/output :alu/position] output)
        (assoc :alu/generator generator))))

(defn flip-x
  "Flip expression on the X axis."
  [expression]
  (let [e (flip-x> (assoc expression :alu/generator identity))
        f (e :alu/generator)]
    (assoc e :alu/pattern (f (e :alu/pattern)))))

(defn wire>
  "Generator for wire."
  [expression distance]
  (if (= (-> expression :alu/output :alu/direction) :bottom-left)
    (flip-x> (wire> (flip-x> expression) distance))
    (let [[x0 y0]    (-> expression :alu/dimensions :alu/origin)
          w          (-> expression :alu/dimensions :alu/width)
          h          (-> expression :alu/dimensions :alu/height)
          p          (-> expression :alu/output :alu/position)
          s          (expression :alu/steps)
          [x y]      (coords/add p [distance distance])
          width      (max w (+ 2 (- x x0)))
          height     (max h (+ 2 (- y y0)))
          steps      (+ s (* 4 distance))]
      (-> expression
          (assoc-in [:alu/dimensions :alu/width] width)
          (assoc-in [:alu/dimensions :alu/height] height)
          (assoc-in [:alu/output :alu/position] [x y])
          (assoc :alu/steps steps)))))

(defn wire
  "Allow transmission of one bit over a distance."
  [expression distance]
  (let [e (wire> (assoc expression :alu/generator identity) distance)
        f (e :alu/generator)]
    (assoc e :alu/pattern (f (e :alu/pattern)))))

(defn shift>
  "Generator for shift."
  [expression offset]
  (let [origin     (-> expression :alu/dimensions :alu/origin)
        position   (-> expression :alu/output :alu/position)
        gen        (expression :alu/generator)
        generator  (fn [args]
                       (patterns/offset (gen args) offset))]
    (-> expression
        (assoc-in [:alu/dimensions :alu/origin] (coords/add origin offset))
        (assoc-in [:alu/output :alu/position] (coords/add position offset))
        (assoc :alu/generator generator))))

(defn shift
  "Shifts an expression with the given offset."
  [expression offset]
  (let [e (shift> (assoc expression :alu/generator identity) offset)
        f (e :alu/generator)]
    (assoc e :alu/pattern (f (e :alu/pattern)))))

(defn align-with-origin>
  "Generator for align-wih-origin"
  [expression]
  (let [[x y]  (-> expression :alu/dimensions :alu/origin)
        offset [(- x) (- y)]]
    (shift> expression offset)))

(defn align-with-origin
  "Aligns the given expression so its origin is [0 0]."
  [expression]
  (let [[e f] (align-with-origin> (assoc expression :alu/generator identity))]
    (assoc e :alu/pattern (f (e :alu/pattern)))))

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

(defn- delay-expression> [left right]
  (let [s1   (left :alu/steps)
        s2   (right :alu/steps)
        diff (int (/ (- s1 s2) 4))]
    (if (pos? diff)
      [left (wire> right diff)]
      [(wire> left (- diff)) right])))

(defn- calculate-offset [l r x-modifier-fn y-modifier-fn]
  (let [x-min     (x-offset-at-origin l r)
        x-diff    (x-offset-at-output l r)
        x         (x-modifier-fn x-min (x-offset-to-right-border l))
        x-offset  (if (odd? (+ x-min x-diff)) (inc x) x)
        y-offset  (y-modifier-fn (y-offset-at-output l r))]
    [x-offset y-offset]))

(defn change-direction>
  "Generator for change-direction"
  [direction expression]
  (let [d (-> expression :alu/output :alu/direction)]
    (if (= d direction) expression (flip-x> expression))))

(defn make-intersect>
  "Generator for make-intersect."
  [left right]
  (let [e1     (change-direction> :bottom-right left)
        e2     (change-direction> :bottom-left right)
        [l r]  (delay-expression> e1 e2)
        offset (calculate-offset l r (fn [x _] x) dec)]
    [l (shift> r offset)]))

(defn make-intersect
  "Aligns expressions by shifting one so that outputs will intersect.
  If both expression have a non zero output, they will cancel each other when intersecting.
  First output will face bottom-right and second output will face bottom-left."
  [left right]
  (let [[l r] (make-intersect> (assoc left :alu/generator identity) (assoc right :alu/generator identity))
        fl    (l :alu/generator)
        fr    (r :alu/generator)]
    [(assoc l :alu/pattern (fl (l :alu/pattern))) (assoc r :alu/pattern (fr (r :alu/pattern)))]))

(defn make-parallel>
  "Generator for make-parallel"
  [left right]
  (let [e1     (change-direction> :bottom-right left)
        e2     (change-direction> :bottom-right right)
        [l r]  (delay-expression> e1 e2)
        offset (calculate-offset l r (fn [x d] (+ x (* 2 d))) identity)]
    [l (shift> r offset)]))

(defn make-parallel
  "Align expressions so they both face bottom right and outputs are synchronised."
  [left right]
  (let [[l r] (make-parallel> (assoc left :alu/generator identity) (assoc right :alu/generator identity))
        fl    (l :alu/generator)
        fr    (r :alu/generator)]
    [(assoc l :alu/pattern (fl (l :alu/pattern))) (assoc r :alu/pattern (fr (r :alu/pattern)))]))

(defn merge-expressions>
  "Generator for merge-expression."
  [& expressions]
  {:pre [(apply = (map #(get-in % [0 :alu/steps]) expressions))]}
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
        steps      ((first expressions) :alu/steps)
        generators (map #(% :alu/generator) expressions)]
    {:alu/dimensions {:alu/origin [x0 y0]
                      :alu/width width
                      :alu/height height}
     :alu/steps steps
     :alu/generator (fn [args] (->> (map vector generators args)
                                    (map #((first %) (last %)))
                                    (reduce set/union)))}))

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

(defn spread-x>
  "Generator for spread-x."
  [& expressions]
  (let [x0          (get-in (first expressions) [:alu/dimensions :alu/origin 0])
        old-origins (map #(get-in % [:alu/dimensions :alu/origin 0]) expressions)
        new-origins (->> (map #(get-in % [:alu/dimensions :alu/width]) expressions)
                         (reductions +)
                         drop-last
                         (cons 0)
                         (map (partial + x0)))
        x-offsets (map #(apply - %) (map vector new-origins old-origins))]
    (map (fn [[e x]] (shift> e [x 0])) (map vector expressions x-offsets))))

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
