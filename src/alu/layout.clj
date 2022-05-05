(ns alu.layout
  (:require [clojure.set :as set]
            [life.patterns :as patterns]
            [life.coords :as coords]))

(defn => [[e f] args]
  (assoc e :alu/pattern (f args)))

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
  [[e f]]
  (let [[x0 _]     (-> e :alu/dimensions :alu/origin)
        width      (-> e :alu/dimensions :alu/width)
        dir        (-> e :alu/output :alu/direction)
        position   (-> e :alu/output :alu/position)
        direction  (if (= dir :bottom-right) :bottom-left :bottom-right)
        output     (coords/flip-x position x0 width)
        expression (-> e
                       (assoc-in [:alu/output :alu/direction] direction)
                       (assoc-in [:alu/output :alu/position] output))
        function   (fn [args]
                     (patterns/flip-x (f args) x0 width))]
    [expression function]))

(defn flip-x
  "Flip expression on the X axis."
  [expression]
  (let [[e f] (flip-x> [expression identity])]
    (assoc e :alu/pattern (f (e :alu/pattern)))))

(defn wire>
  "Generator for wire."
  [[e f] distance]
  (if (= (-> e :alu/output :alu/direction) :bottom-left)
    (flip-x> (wire> (flip-x> [e f]) distance))
    (let [[x0 y0]    (-> e :alu/dimensions :alu/origin)
          w          (-> e :alu/dimensions :alu/width)
          h          (-> e :alu/dimensions :alu/height)
          p          (-> e :alu/output :alu/position)
          s          (e :alu/steps)
          [x y]      (coords/add p [distance distance])
          width      (max w (+ 2 (- x x0)))
          height     (max h (+ 2 (- y y0)))
          steps      (+ s (* 4 distance))
          expression (-> e
                         (assoc-in [:alu/dimensions :alu/width] width)
                         (assoc-in [:alu/dimensions :alu/height] height)
                         (assoc-in [:alu/output :alu/position] [x y])
                         (assoc :alu/steps steps))]
      [expression f])))

(defn wire
  "Allow transmission of one bit over a distance."
  [expression distance]
  (let [[e f] (wire> [expression identity] distance)]
    (assoc e :alu/pattern (f (e :alu/pattern)))))

(defn shift>
  "Generator for shift."
  [[e f] offset]
  (let [origin     (-> e :alu/dimensions :alu/origin)
        position   (-> e :alu/output :alu/position)
        expression (-> e
                       (assoc-in [:alu/dimensions :alu/origin] (coords/add origin offset))
                       (assoc-in [:alu/output :alu/position] (coords/add position offset)))
        function   (fn [args]
                       (patterns/offset (f args) offset))]
    [expression function]))

(defn shift
  "Shifts an expression with the given offset."
  [expression offset]
  (let [[e f] (shift> [expression identity] offset)]
    (assoc e :alu/pattern (f (e :alu/pattern)))))


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

(defn- delay-expression> [[e1 f1] [e2 f2]]
  (let [s1   (e1 :alu/steps)
        s2   (e2 :alu/steps)
        diff (int (/ (- s1 s2) 4))]
    (if (pos? diff)
      [[e1 f1] (wire> [e2 f2] diff)]
      [(wire> [e1 f1] (- diff)) [e2 f2]])))

(defn- delay-expression [left right]
  (let [[[e1 f1] [e2 f2]] (delay-expression> [left identity] [right identity])]
    [(assoc e1 :alu/pattern (f1 (e1 :alu/pattern))) (assoc e2 :alu/pattern (f2 (e2 :alu/pattern)))]))

(defn- calculate-offset [l r x-modifier-fn y-modifier-fn]
  (let [x-min     (x-offset-at-origin l r)
        x-diff    (x-offset-at-output l r)
        x         (x-modifier-fn x-min (x-offset-to-right-border l))
        x-offset  (if (odd? (+ x-min x-diff)) (inc x) x)
        y-offset  (y-modifier-fn (y-offset-at-output l r))]
    [x-offset y-offset]))

(defn change-direction>
  "Generator for change-direction"
  [direction [e f]]
  (let [d (-> e :alu/output :alu/direction)]
    (if (= d direction) [e f] (flip-x> [e f]))))

(defn change-direction
  "Change direction of expression."
  [direction expression]
  (let [[e f] (change-direction> direction[expression identity])]
    (assoc e :alu/pattern (f (e :alu/pattern)))))

(defn make-intersect>
  "Generator for make-intersect."
  [[e1 f1] [e2 f2]]
  (let [left            (change-direction> :bottom-right [e1 f1])
        right           (change-direction> :bottom-left [e2 f2])
        [[l fl] [r fr]] (delay-expression> left right)
        offset          (calculate-offset l r (fn [x _] x) dec)]
    [[l fl] (shift> [r fr] offset)]))

(defn make-intersect
  "Aligns expressions by shifting one so that outputs will intersect.
  If both expression have a non zero output, they will cancel each other when intersecting.
  First output will face bottom-right and second output will face bottom-left."
  [left right]
  (let [[[e1 f1] [e2 f2]] (make-intersect> [left identity] [right identity])]
    [(assoc e1 :alu/pattern (f1 (e1 :alu/pattern))) (assoc e2 :alu/pattern (f2 (e2 :alu/pattern)))]))

(defn make-parallel>
  "Generator for make-parallel"
  [[e1 f1] [e2 f2]]
  (let [left            (change-direction> :bottom-right [e1 f1])
        right           (change-direction> :bottom-right [e2 f2])
        [[l fl] [r fr]] (delay-expression> left right)
        offset          (calculate-offset l r (fn [x d] (+ x (* 2 d))) identity)]
    [[l fl] (shift> [r fr] offset)]))

(defn make-parallel
  "Align expressions so they both face bottom right and outputs are synchronised."
  [left right]
  (let [[[e1 f1] [e2 f2]] (make-parallel> [left identity] [right identity])]
    [(assoc e1 :alu/pattern (f1 (e1 :alu/pattern))) (assoc e2 :alu/pattern (f2 (e2 :alu/pattern)))]))

(defn merge-expressions>
  "Generator for merge-expression."
  [& args]
  {:pre [(apply = (map #(get-in % [0 :alu/steps]) args))]}
  (let [expressions (map #(first %) args)
        functions   (map #(last %) args)
        xs      (map #(get-in % [:alu/dimensions :alu/origin 0]) expressions)
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
        expression {:alu/dimensions {:alu/origin [x0 y0]
                                     :alu/width width
                                     :alu/height height}
                    :alu/steps steps}
        function (fn [args] (->> (map vector functions args)
                                 (map #((first %) (last %)))
                                 (reduce set/union)))]
    [expression function]))

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
