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
  (let [{:keys [steps pattern]
         {:keys [origin width height]} :dimensions
         {:keys [position direction]} :output} expression
        new-output (coords/add position (vector distance distance))]
    (if (= direction :bottom-left)
      (flip-x (wire (flip-x expression) distance))
      {:dimensions {:origin origin
                    :width (max width (+ 2 (- (first new-output) (first origin))))
                    :height (max height (+ 2 (- (second new-output) (second origin))))}
       :output {:position new-output
                :direction direction}
       :steps (+ steps (* 4 distance))
       :pattern pattern})))
