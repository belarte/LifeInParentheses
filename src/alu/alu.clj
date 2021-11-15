(ns alu.alu
  (:require [alu.layout :as layout]
            [life.life :as life]
            [life.patterns :as patterns]))

(defn bit
  "Represents a single bit as an input to an expression."
  [value]
  {:pre [(or (= 1 value) (= 0 value))]}
  (let [pattern (if (zero? value) #{} (patterns/offset patterns/glider [1 1]))]
    {:dimensions {:origin [0 0]
                  :width 5
                  :height 5}
     :output {:position [3 3]
              :direction :bottom-right}
     :steps 0
     :pattern pattern}))

(defn output
  "Reads a single bit as the output of an expression."
  [expression]
  (let [{:keys [dimensions steps output pattern]} expression
        board (life/create-board (dimensions :width) (dimensions :height) [pattern])
        iterations (life/simulate board steps)
        last-iteration (last iterations)]
    (if (contains? (last-iteration :alive-cells) (output :position)) 1 0)))

(comment
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
  (output (bit 1)))
