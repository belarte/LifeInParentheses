(ns life.components.canvas
  (:require [reagent.core :as r]))

(defonce canvas-ref (r/atom nil))

(defonce steps (r/atom []))

(def size 5)

(defn- draw []
  (let [ctx (.getContext @canvas-ref "2d")]
    (run!
      (fn [[x y]] (.fillRect ctx (* x size) (* y size) size size))
      (get @steps 0))))

(defn canvas [w h s]
  (reset! steps s)
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
             :on-click (fn [] (draw))}]]])

(comment
  (.log js/console (clj->js @steps)))
