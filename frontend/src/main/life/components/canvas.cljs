(ns life.components.canvas
  (:require [reagent.core :as r]))

(defonce canvas-ref (r/atom nil))
(defonce steps (r/atom []))
(defonce counter (r/atom 0))

(def size 5)

(defn- next-step []
  (let [last-index (- (count @steps) 1)
        index      (min @counter last-index)]
    (get @steps index)))

(defn draw []
  (when (not= nil @canvas-ref)
    (let [ctx (.getContext @canvas-ref "2d")]
      (.clearRect ctx 0 0 (.-width @canvas-ref) (.-height @canvas-ref))
      (run!
        (fn [[x y]] (.fillRect ctx (* x size) (* y size) size size))
        (next-step)))))

(defn canvas [w h s]
  (reset! steps s)
  (reset! counter 0)
  [:div
   [:div
    [:canvas {:id "canvas"
              :role "canvas-role"
              :width (* size w)
              :height (* size h)
              :ref (fn [e] (reset! canvas-ref e))
              :style {:border "1px solid black"}}]]
   [:div
    [:input {:type "button"
             :value "Draw"
             :on-click (fn [] (swap! counter inc))}]]])

(comment
  (.log js/console (clj->js @steps)))
