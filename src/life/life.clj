(ns life)

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

(defn create []
  (fn [board] board))
