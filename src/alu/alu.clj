(ns alu.alu
  (:require [life.life :as life]
            [life.patterns :as patterns]))

(defn bit
  "Represents a single bit as an input to an expression."
  [value]
  {:pre [(or (= 1 value) (= 0 value))]}
  (let [pattern (if (zero? value) #{} (life/offset patterns/glider [1 1]))]
    {:origin [0 0]
     :width 5
     :height 5
     :output [3 3]
     :steps 0
     :pattern pattern}))

(defn output
  "Reads a single bit as the output of an expression."
  [expression]
  (let [{:keys [width height steps output pattern]} expression
        board (life/create-board width height [pattern])
        iterations (life/simulate board steps)
        last-iteration (last iterations)]
    (if (contains? (last-iteration :alive-cells) output) 1 0)))

(defn wire
  "Allow transmission of one bit over a distance."
  [expression distance]
  (let [{:keys [origin width height output pattern]} expression
        new-output (life/add-coords output (vector distance distance))]
    {:origin origin
     :width (max width (+ 2 (- (first output) (first origin))))
     :height (max height (+ 2 (- (second output) (second origin))))
     :output new-output
     :steps (* 4 distance)
     :pattern pattern}))

(comment
  (let [test (wire (bit 1) 3)
        board (life/create-board (test :width) (test :height) [(test :pattern)])]
    (println (life/draw-board board)))
  (output (bit 1)))
