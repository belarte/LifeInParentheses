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
      (set (for [[loc n] (frequencies (mapcat neighbours (board :alive-cells)))
                 :when (if ((board :alive-cells) loc) (survive? n) (birth? n))]
             loc))}))

(def conway-stepper (stepper neighbours #{3} #{2 3}))

(defn- extract-x-axis [entry]
  (->> entry
       (map first)
       (set)))

(defn- prepare-output [board]
  (->> (board :alive-cells)
       (group-by second)
       (map (fn [[key value]] [key (extract-x-axis value)]))
       (into {})))

(defn- draw-line [width array]
  (str/join (map #(if (contains? array %) "#" "-") (range width))))

(defn draw-board [board]
  (let [y-axis (range (board :height))
        output (prepare-output board)]
    (str/join \newline (map #(if (contains? output %)
                              (draw-line (board :width) (output %))
                              (str/join (repeat (board :width) "-"))) y-axis))))

(defn offset [pattern coordinate]
  (set (map #(vector (+ (first %) (first coordinate)) (+ (second %) (second coordinate))) pattern)))

(comment
  (offset (patterns :glider) [1 2])
  (stepper neighbours #{3} #{2 3})
  (conway-stepper (create-board 10 10 [(patterns :eater)]))
  (println (draw-board (create-board 9 6 [(patterns :glider)])))
  (println (draw-board (conway-stepper (create-board 9 6 [(patterns :glider)]))))
  (prepare-output (create-board 10 10 [(patterns :eater)])))
