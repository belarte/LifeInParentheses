(ns alu.alu
  (:require [life.life :as life]
            [life.patterns :as patterns]))

(defn bit
  "Represents a single bit as an input to an expression."
  [value]
  {:pre [(or (= 1 value) (= 0 value))]}
  (let [pattern (if (zero? value) #{} (life/offset patterns/glider [1 1]))]
    {:width 5
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

(comment
  (output (bit 1)))
