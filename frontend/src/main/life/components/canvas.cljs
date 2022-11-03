(ns life.components.canvas
  (:require [reagent.core :as r]
            [reagent-modals.modals :as rm]
            [life.components.settings :as s]))

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
    [:input {:type "button"
             :value "Prev"
             :on-click backward}]
    [:input {:type "button"
             :value (if (running?) "Stop" "Start")
             :on-click #(if (running?) (stop) (start))}]
    [:input {:type "button"
             :value "Next"
             :on-click forward}]
    [:input {:type "button"
             :value "Reset"
             :on-click reset}]
    [:input {:type "button"
             :value "Settings"
             :on-click (fn []
                         (stop)
                         (rm/modal! [s/settings]))}]]])

(comment
  (reset)
  (start)
  (stop)
  (.log js/console (clj->js @steps)))
