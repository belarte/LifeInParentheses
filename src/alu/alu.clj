(ns alu.alu
  (:require [clojure.spec.alpha :as s]
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

(defn output
  "Reads a single bit as the output of an expression."
  [expression]
  {:pre [(s/valid? :alu/expression expression) (layout/within-bounds? expression)]}
  (let [{:keys [alu/dimensions alu/steps alu/output alu/pattern]} expression
        board (life/create-board (dimensions :alu/width) (dimensions :alu/height) [pattern])
        iterations (life/simulate board steps)
        last-iteration (last iterations)]
    (if (contains? (last-iteration :alive-cells) (output :alu/position)) 1 0)))

(defn print-e
  "Prints all steps generated."
  [expression]
  {:pre [(s/valid? :alu/expression expression) (layout/within-bounds? expression)]}
  (let [{:keys [alu/dimensions alu/steps alu/pattern]} expression
        board (life/create-board (dimensions :alu/width) (dimensions :alu/height) [pattern])
        iterations (life/simulate board steps)]
    (run! #(do (println %) (println (life/draw-board %))) iterations)))

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
            [x-lo _] (get-in l [:alu/output :alu/position])
            [x-ro y-ro] (get-in r [:alu/output :alu/position])
            x-diff (+ (int (/ (- x-ro x-lo) 2)) 5)
            steps (+ (r :alu/steps) (* 4 x-diff))]
        (-> (layout/merge-expressions l r)
            (assoc-in [:alu/output :alu/direction] :bottom-left)
            (assoc-in [:alu/output :alu/position] (coords/add [x-ro y-ro] [(- x-diff) x-diff]))
            (assoc :alu/steps steps))))))

(comment
  (s/explain :alu/expression (bit 1))
  (print-e (not-e (layout/wire (bit 1) 3)))
  (let [one (bit 1)
        flipped (layout/flip-x one)
        wired (layout/wire flipped 2)
        b1 (life/create-board ((one :dimensions) :width) ((one :dimensions) :height) [(one :pattern)])
        b2 (life/create-board ((flipped :dimensions) :width) ((flipped :dimensions) :height) [(flipped :pattern)])
        b3 (life/create-board ((wired :dimensions) :width) ((wired :dimensions) :height) [(wired :pattern)])]
    (println one)
    (println (life/draw-board b1))
    (println flipped)
    (println (life/draw-board b2))
    (println wired)
    (println (life/draw-board b3)))
  (output (bit 1))
  (layout/within-bounds? (bit 1)))
