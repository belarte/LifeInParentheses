(ns life
  (:require [clojure.set :as set]
            [clojure.string :as str]))

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

(defn create []
  (fn [board] board))

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
  (println (draw-world (create-world 9 6 [pattern-eater])))
  (prepare-output pattern-eater))
