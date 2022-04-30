(ns alu.alu
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [alu.layout :as layout]
            [life.coords :as coords]
            [life.life :as life]
            [life.patterns :as patterns]))

(s/def :alu/origin :coords/coords)
(s/def :alu/width (s/and int? pos?))
(s/def :alu/height (s/and int? pos?))
(s/def :alu/dimensions (s/keys :req [:alu/origin :alu/width :alu/height]))

(s/def :alu/position :coords/coords)
(s/def :alu/direction #{:bottom-left :bottom-right})
(s/def :alu/output (s/keys :req [:alu/position :alu/direction]))

(s/def :alu/steps (s/and int? (s/or :pos pos? :zero zero?)))
(s/def :alu/pattern :patterns/pattern)

(s/def :alu/expression (s/keys :req [:alu/dimensions
                                     :alu/output
                                     :alu/steps
                                     :alu/pattern]))

(defn bit
  "Represents a single bit as an input to an expression."
  [value]
  {:pre [(or (= 1 value) (= 0 value))]
   :post [(s/valid? :alu/expression %) (layout/within-bounds? %)]}
  (let [pattern (if (zero? value) #{} (patterns/offset patterns/glider [1 1]))]
    {:alu/dimensions {:alu/origin [0 0]
                      :alu/width 5
                      :alu/height 5}
     :alu/output {:alu/position [3 3]
                  :alu/direction :bottom-right}
     :alu/steps 0
     :alu/pattern pattern}))

(defn- evaluate [expression]
  (let [{:keys [alu/dimensions alu/steps alu/pattern]} expression
        board (life/create-board (dimensions :alu/width) (dimensions :alu/height) [pattern])]
    (life/simulate board steps)))

(defn read-bit
  "Reads a single bit as the output of an expression."
  [expression]
  {:pre [(s/valid? :alu/expression expression) (layout/within-bounds? expression)]}
  (let [output (get-in expression [:alu/output :alu/position])
        last-iteration (last (evaluate expression))]
    (if (contains? (last-iteration :alive-cells) output) 1 0)))

(defn print-e
  "Prints all steps generated."
  [expression]
  {:pre [(s/valid? :alu/expression expression) (layout/within-bounds? expression)]}
  (run! #(do (println %) (println (life/draw-board %) (Thread/sleep 100)))
        (evaluate expression)))

(defn- get-intersection [left right]
  (let [[x0 y0] (-> left :alu/output :alu/position)
        [x1 __] (-> right :alu/output :alu/position)
        diff    (int (/ (- x1 x0) 2))]
      [(+ x0 diff) (+ y0 diff) diff]))

(defn not-bit
  "Negates a single bit. Expects the input to be facing bottom right, if not the input will be flipped."
  [expression]
  {:pre [(s/valid? :alu/expression expression) (layout/within-bounds? expression)]
   :post [(s/valid? :alu/expression %) (layout/within-bounds? %)]}
  (let [[l r]       (layout/make-intersect expression (bit 1))
        [x y diff]  (get-intersection l r)
        height-diff (+ diff 3 1)
        height      (+ (-> l :alu/dimensions :alu/height) height-diff)
        steps-diff  (* 4 (+ diff 4))
        steps       (+ (r :alu/steps) steps-diff)
        output-pos  (coords/add [x y] [(- 4) 3])]
    (-> (layout/merge-expressions l r)
        (assoc-in [:alu/dimensions :alu/height] height)
        (assoc-in [:alu/output :alu/direction] :bottom-left)
        (assoc-in [:alu/output :alu/position] output-pos)
        (assoc :alu/steps steps))))

(defn and-bit
  "Combine left and right expressions to form an 'and' statement."
  [left right]
  {:pre  [(s/valid? :alu/expression left) (layout/within-bounds? left)
          (s/valid? :alu/expression right) (layout/within-bounds? right)]
   :post [(s/valid? :alu/expression %) (layout/within-bounds? %)]}
  (let [[l-a r-a]   (layout/make-parallel left right)
        [_ n]       (layout/make-intersect r-a (bit 1))
        [x y diff]  (get-intersection l-a n)
        height-diff (+ diff 6 1)
        height      (+ (-> l-a :alu/dimensions :alu/height) height-diff)
        steps-diff  (* 4 (+ diff 6))
        steps       (+ (n :alu/steps) steps-diff)
        output-pos  (coords/add [x y] [6 6])
        eater-pos   (coords/add [x y] [-6 3])
        eater       (patterns/offset (patterns/flip-x patterns/eater) eater-pos)]
      (-> (layout/merge-expressions l-a (layout/merge-expressions r-a n))
          (assoc-in [:alu/dimensions :alu/height] height)
          (assoc-in [:alu/output :alu/direction] :bottom-right)
          (assoc-in [:alu/output :alu/position] output-pos)
          (assoc :alu/steps steps)
          (update :alu/pattern set/union eater))))

(defn or-bit
  "Combine left and right expressions to form an 'or' statement."
  [left right]
  {:pre [(s/valid? :alu/expression left) (layout/within-bounds? left)
         (s/valid? :alu/expression right) (layout/within-bounds? right)]
   :post [(s/valid? :alu/expression %) (layout/within-bounds? %)]}
  (not-bit (and-bit (not-bit left) (not-bit right))))

(comment
  (s/explain :alu/expression (bit 1))
  (print-e (not-bit (layout/wire (bit 1) 3)))
  (print-e (layout/align-with-origin (and-bit (bit 1) (bit 1))))
  (let [exp (layout/align-with-origin (and-bit (bit 1) (and-bit (bit 1) (bit 1))))
        board (life/create-board ((exp :alu/dimensions) :alu/width) ((exp :alu/dimensions) :alu/height) [(exp :alu/pattern)])]
    (println exp)
    (println (life/draw-board board))
    (print-e exp))
  (read-bit (bit 1))
  (layout/within-bounds? (bit 1)))
