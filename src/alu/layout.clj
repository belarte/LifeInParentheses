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
