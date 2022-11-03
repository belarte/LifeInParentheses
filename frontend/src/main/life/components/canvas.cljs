(ns life.components.canvas
  (:require [reagent.core :as r]
            [reagent-modals.modals :as rm]
            [life.components.settings :as s]
            [reagent-mui.material.icon-button :refer [icon-button]]
            [reagent-mui.icons.play-circle-outline :refer [play-circle-outline]]
            [reagent-mui.icons.pause-circle-outline :refer [pause-circle-outline]]
            [reagent-mui.icons.skip-previous :refer [skip-previous]]
            [reagent-mui.icons.skip-next :refer [skip-next]]
            [reagent-mui.icons.replay :refer [replay]]
            [reagent-mui.icons.settings :refer [settings]]))

(defonce canvas-ref (r/atom nil))
(defonce steps (r/atom []))
(defonce counter (r/atom 0))
(defonce interval-id (r/atom nil))

(defn- last-index []
  (- (count @steps) 1))

(defn- next-step []
  (let [index (min @counter (last-index))]
    (get @steps index)))

(defn draw []
  (when (not= nil @canvas-ref)
    (let [ctx  (.getContext @canvas-ref "2d")
          size @s/size]
      (.clearRect ctx 0 0 (.-width @canvas-ref) (.-height @canvas-ref))
      (run!
        (fn [[x y]] (.fillRect ctx (* x size) (* y size) size size))
        (next-step)))))

(defn- forward []
  (swap! counter (fn [n] (if (< n (last-index)) (inc n) n))))

(defn- backward []
  (swap! counter (fn [n] (if (> n 0) (dec n) n))))

(defn- start []
  (reset! interval-id (js/setInterval #(swap! counter forward) @s/interval)))

(defn- stop []
  (js/clearInterval @interval-id)
  (reset! interval-id nil))

(defn- reset []
  (reset! counter 0))

(defn- running? []
  (not= nil @interval-id))

(defn- button [label on-click icon]
  [icon-button {:aria-label label
                :size       "large"
                :on-click   on-click}
   [icon {:font-size "inherit"}]])

(defn canvas [w h s]
  (reset! steps s)
  [:div
   [:div
    [:canvas {:id "canvas"
              :role "canvas-role"
              :width (* @s/size w)
              :height (* @s/size h)
              :ref (fn [e] (reset! canvas-ref e))
              :style {:border "1px solid black"}}]]
   [:div
    [button "back-button" backward skip-previous]
    [button
      (if (running?) "pause-button" "play-button")
      #(if (running?) (stop) (start))
      (if (running?) pause-circle-outline play-circle-outline)]
    [button "next-button" forward skip-next]
    [button "reset-button" reset replay]
    [button
      "settings-button"
      (fn []
        (stop)
        (rm/modal! [s/settings]))
      settings]]])

(comment
  (reset)
  (start)
  (stop)
  (.log js/console (clj->js @steps)))
