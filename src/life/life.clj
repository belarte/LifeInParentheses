(ns life.life
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def patterns
  {
   ;; -o-
   ;; --o
   ;; ooo
   :glider
   #{[1 0] [2 1] [0 2] [1 2] [2 2]}

   ;; ---oo--oo
   ;; o---o--oo
   ;; oooo-----
   ;; ---------
   ;; oo-------
   ;; oo-------
   :eater
   #{[3 0] [4 0] [7 0] [8 0]
     [0 1] [4 1] [7 1] [8 1]
     [0 2] [1 2] [2 2] [3 2]
     [0 4] [1 4] [0 5] [1 5]}

   ;; oo
   ;; oo
   :square
   #{[0 0] [1 0] [0 1] [1 1]}})

(defn create-board [width height patterns]
  {:width width
   :height height
   :alive-cells (apply set/union patterns)})

(defn- neighbours
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]))

(defn- stepper
  [neighbours birth? survive?]
  (fn [board]
    {:width (board :width)
     :height (board :height)
     :alive-cells
      (set (for [[coord n] (frequencies (mapcat neighbours (board :alive-cells)))
                 :when (if ((board :alive-cells) coord) (survive? n) (birth? n))]
             coord))}))

(def conway-stepper (stepper neighbours #{3} #{2 3}))

(defn simulate [board iterations]
  (->> (iterate conway-stepper board)
       (take (inc iterations))
       (last)))

(defn shape-output [board alive dead]
  (for [y (range (board :height))]
    (for [x (range (board :width))]
      (if (contains? (board :alive-cells) [x y]) alive dead))))

(defn draw-board [board]
  (let [output (shape-output board "#" "-")]
    (str/join \newline (map str/join output))))

(defn offset [pattern coordinate]
  (set (map #(vector (+ (first %) (first coordinate)) (+ (second %) (second coordinate))) pattern)))

(defn flip-x [pattern]
  (let [max-x (apply max (map first pattern))]
    (set (map #(vector (- max-x (first %)) (second %)) pattern))))

(comment
  (let [board (create-board 4 4 [(patterns :glider)])]
    (shape-output board "#" "-"))
  (offset (patterns :glider) [1 2])
  (stepper neighbours #{3} #{2 3})
  (conway-stepper (create-board 10 10 [(patterns :eater)]))
  (println (draw-board (create-board 9 6 [(patterns :glider)])))
  (println (draw-board (create-board 9 6 [(flip-x (patterns :eater))])))
  (println (draw-board (conway-stepper (create-board 9 6 [(patterns :glider)])))))
