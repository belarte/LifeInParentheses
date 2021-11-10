(ns alu.alu
  (:require [life.life :as life]
            [life.coords :as coords]
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

(defn wire
  "Allow transmission of one bit over a distance."
  [expression distance]
  (let [{:keys [steps pattern]
         {:keys [origin width height]} :dimensions
         {:keys [position direction]} :output} expression
        new-output (coords/add position (vector distance distance))]
    {:dimensions {:origin origin
                  :width (max width (+ 2 (- (first new-output) (first origin))))
                  :height (max height (+ 2 (- (second new-output) (second origin))))}
     :output {:position new-output
              :direction direction}
     :steps (+ steps (* 4 distance))
     :pattern pattern}))

(comment
  (let [test (wire (bit 1) 3)
        board (life/create-board (test :width) (test :height) [(test :pattern)])]
    (println (life/draw-board board)))
  (output (bit 1)))
