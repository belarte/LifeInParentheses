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
  (let [board (life/create-board (expression :width) (expression :height) [(expression :pattern)])
        steps (life/simulate board (expression :steps))
        last (last steps)]
    (if (contains? (last :alive-cells) (expression :output)) 1 0)))

(defn wire
  "Allow transmission of one bit over a distance."
  [expression distance]
  (let [output (life/add-coords (expression :output) (vector distance distance))]
    {:origin (expression :origin)
     :width (max (expression :width) (+ 2 (- (first output) (first (expression :origin)))))
     :height (max (expression :height) (+ 2 (- (second output) (second (expression :origin)))))
     :output output
     :steps (* 4 distance)
     :pattern (expression :pattern)}))

(comment
  (let [test (wire (bit 1) 3)
        board (life/create-board (test :width) (test :height) [(test :pattern)])]
    (println (life/draw-board board)))
  (output (bit 1)))
