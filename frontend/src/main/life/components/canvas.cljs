(ns life.components.canvas
  (:require [reagent.core :as r]))

(defonce canvas-ref (r/atom nil))

(defn- draw []
  (let [ctx (.getContext @canvas-ref "2d")]
    (.fillRect ctx 10 10 10 10)))

(defn canvas [w h]
  [:div
   [:div
    [:canvas {:id "canvas"
              :role "canvas-role"
              :width w
              :height h
              :ref (fn [e] (reset! canvas-ref e))
              :style {:border "1px solid black"}}]]
   [:div
    [:input {:type "button"
             :value "Draw"
             :on-click (fn [] (draw))}]]])
