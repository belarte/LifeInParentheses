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
  (run! #(do (println %) (println (life/draw-board %)))
        (evaluate expression)))

(defn not-e
  "Negates an expression."
  [expression]
  {:pre [(s/valid? :alu/expression expression) (layout/within-bounds? expression)]
   :post [(s/valid? :alu/expression %) (layout/within-bounds? %)]}
  (let [direction (get-in expression [:alu/output :alu/direction])]
    (if (= direction :bottom-left)
      (layout/flip-x (not-e (layout/flip-x expression)))
      (let [complement (layout/flip-x (bit 1))
            [l r] (layout/align-for-intersection expression complement)
            [x-lo y-lo] (get-in l [:alu/output :alu/position])
            [x-ro y-ro] (get-in r [:alu/output :alu/position])
            x-diff (+ (int (/ (- x-ro x-lo) 2)) 5)
            height (+ y-lo x-diff 1)
            steps (+ (r :alu/steps) (* 4 x-diff))]
        (-> (layout/merge-expressions l r)
            (assoc-in [:alu/dimensions :alu/height] height)
            (assoc-in [:alu/output :alu/direction] :bottom-left)
            (assoc-in [:alu/output :alu/position] (coords/add [x-ro y-ro] [(- x-diff) x-diff]))
            (assoc :alu/steps steps))))))

(defn and-e
  "Combine left and right expressions to form an 'and' statement."
  [left right]
  {:pre [(s/valid? :alu/expression left) (layout/within-bounds? left)
         (s/valid? :alu/expression right) (layout/within-bounds? right)
         (= (get-in left [:alu/output :alu/direction]) (get-in right [:alu/output :alu/direction]))]
   :post [(s/valid? :alu/expression %) (layout/within-bounds? %)]}
  (let [direction (get-in left [:alu/output :alu/direction])]
    (if (= direction :bottom-left)
      (layout/flip-x (and-e (layout/flip-x left) (layout/flip-x right)))
      (let [not-right (not-e right)
            [l r] (layout/align-for-intersection left not-right)
            [x-lo y-lo] (get-in l [:alu/output :alu/position])
            [x-ro _] (get-in r [:alu/output :alu/position])
            x-diff (+ (int (/ (- x-ro x-lo) 2)) 7)
            height (+ y-lo x-diff 1)
            steps (+ (r :alu/steps) (* 4 x-diff))
            eater (patterns/offset (patterns/flip-x patterns/eater) [(- x-diff 3) (+ x-diff 6)])]
        (-> (layout/merge-expressions l r)
            (assoc-in [:alu/dimensions :alu/height] height)
            (assoc-in [:alu/output :alu/direction] :bottom-right)
            (assoc-in [:alu/output :alu/position] (coords/add [x-lo y-lo] [x-diff x-diff]))
            (assoc :alu/steps steps)
            (update :alu/pattern set/union eater))))))

(defn or-e
  "Combine left and right expressions to form an 'or' statement."
  [left right]
  {:pre [(s/valid? :alu/expression left) (layout/within-bounds? left)
         (s/valid? :alu/expression right) (layout/within-bounds? right)
         (= (get-in left [:alu/output :alu/direction]) (get-in right [:alu/output :alu/direction]))]
   :post [(s/valid? :alu/expression %) (layout/within-bounds? %)]}
  (not-e (and-e (not-e left) (not-e right))))

(comment
  (s/explain :alu/expression (bit 1))
  (print-e (not-e (layout/wire (bit 1) 3)))
  (print-e (and-e (bit 0) (bit 0)))
  (let [exp (and-e (bit 1) (bit 1))
        board (life/create-board ((exp :alu/dimensions) :alu/width) ((exp :alu/dimensions) :alu/height) [(exp :alu/pattern)])]
    (println exp)
    (println (life/draw-board board)))
  (read-bit (bit 1))
  (layout/within-bounds? (bit 1)))
