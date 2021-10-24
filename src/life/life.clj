(ns life
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def pattern-glider
  ;; -o-
  ;; --o
  ;; ooo
  {:width 3
   :height 3
   :alive-cells #{[1 0] [2 1] [0 2] [1 2] [2 2]}})

(def pattern-eater
  ;; ---oo--oo
  ;; o---o--oo
  ;; oooo-----
  ;; ---------
  ;; oo-------
  ;; oo-------
  {:width 9
   :height 6
   :alive-cells #{[3 0] [4 0] [7 0] [8 0]
                  [0 1] [4 1] [7 1] [8 1]
                  [0 2] [1 2] [2 2] [3 2]
                  [0 4] [1 4] [0 5] [1 5]}})

(def pattern-square
  ;; oo
  ;; oo
  {:width 2
   :height 2
   :alive-cells #{[0 0] [1 0] [0 1] [1 1]}})

(defn create-world [width height patterns]
  (let [cells (->> patterns
                   (map #(% :alive-cells))
                   (apply set/union))]
    {:width width
     :height height
     :alive-cells cells}))

(defn neighbours
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]))

(defn stepper
  [neighbours birth? survive?]
  (fn [board]
    {:width (board :width)
     :height (board :height)
     :alive-cells
      (set (for [[loc n] (frequencies (mapcat neighbours (board :alive-cells)))
                 :when (if ((board :alive-cells) loc) (survive? n) (birth? n))]
             loc))}))

(def conway-stepper (stepper neighbours #{3} #{2 3}))

(defn extract-x-axis [entry]
  (->> entry
       (map first)
       (set)))

(defn prepare-output [board]
  (->> (board :alive-cells)
       (group-by second)
       (map (fn [[key value]] [key (extract-x-axis value)]))
       (into {})))

(defn draw-line [width array]
  (str/join (map #(if (contains? array %) "#" "-") (range width))))

(defn draw-world [board]
  (let [y-axis (range (board :height))
        output (prepare-output board)]
    (str/join \newline (map #(if (contains? output %)
                              (draw-line (board :width) (output %))
                              (str/join (repeat (board :width) "-"))) y-axis))))

(comment
  (stepper neighbours #{3} #{2 3})
  (conway-stepper pattern-eater)
  (println (draw-world (create-world 9 6 [pattern-glider])))
  (println (draw-world (conway-stepper (create-world 9 6 [pattern-glider]))))
  (prepare-output pattern-eater))
