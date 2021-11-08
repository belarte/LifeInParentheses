(ns life.life
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [life.coords :as coords]
            [life.patterns :as patterns]))

(defn create-board
  "Creates a Game Of Life board. Requires a sequence of patterns."
  [width height patterns]
  {:width width
   :height height
   :alive-cells (apply set/union patterns)})

(defn- stepper
  [neighbours birth? survive?]
  (fn [board]
    {:width (board :width)
     :height (board :height)
     :alive-cells
      (set (for [[coord n] (frequencies (mapcat neighbours (board :alive-cells)))
                 :when (if ((board :alive-cells) coord) (survive? n) (birth? n))]
             coord))}))

(def conway-stepper
  "Returns a default Conway's Game Of Life step function."
  (stepper coords/neighbours #{3} #{2 3}))

(defn simulate
  "Simulates n iterations of Conway's Game Of Life. Returns a sequence of all steps, including the initial board."
  [board iterations]
  (->> (iterate conway-stepper board)
       (take (inc iterations))))

(defn- shape-output [board alive dead]
  (for [y (range (board :height))]
    (for [x (range (board :width))]
      (if (contains? (board :alive-cells) [x y]) alive dead))))

(defn draw-board
  "Draw the board as a multi-lines string."
  [board]
  (let [output (shape-output board "#" "-")]
    (str/join \newline (map str/join output))))

(defn offset
  "Offset a pattern with the given coordinate."
  [pattern coordinate]
  (set (map (partial coords/add coordinate) pattern)))

(defn flip-x
  "Flip a pattern on the X axis."
  ([pattern]
   (let [x-max (apply max (map first pattern))]
     (flip-x pattern 0 (inc x-max))))
  ([pattern x0 w]
   (set (map #(coords/flip-x % x0 w) pattern))))

(comment
  (let [board (create-board 4 4 [patterns/glider])]
    (shape-output board "#" "-"))
  (let [board (create-board 9 9 [patterns/glider])]
    (->> (simulate board 16)
         (map draw-board)
         (map println)))
  (offset patterns/glider [1 2])
  (stepper coords/neighbours #{3} #{2 3})
  (conway-stepper (create-board 10 10 [patterns/eater]))
  (println (draw-board (create-board 9 6 [patterns/glider])))
  (println (draw-board (create-board 9 6 [(flip-x patterns/eater)])))
  (println (draw-board (conway-stepper (create-board 9 6 [patterns/glider])))))
