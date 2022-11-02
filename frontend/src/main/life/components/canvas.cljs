(ns life.components.canvas
  (:require [reagent.core :as r]
            [reagent-modals.modals :as rm]))

(defonce canvas-ref (r/atom nil))
(defonce steps (r/atom []))
(defonce counter (r/atom 0))
(defonce interval-id (r/atom nil))

(defonce size (r/atom 5))
(defonce interval (r/atom 1000))

(defn- last-index []
  (- (count @steps) 1))

(defn- next-step []
  (let [index (min @counter (last-index))]
    (get @steps index)))

(defn draw []
  (when (not= nil @canvas-ref)
    (let [ctx (.getContext @canvas-ref "2d")]
      (.clearRect ctx 0 0 (.-width @canvas-ref) (.-height @canvas-ref))
      (run!
        (fn [[x y]] (.fillRect ctx (* x @size) (* y @size) @size @size))
        (next-step)))))

(defn- forward []
  (swap! counter (fn [n] (if (< n (last-index)) (inc n) n))))

(defn- backward []
  (swap! counter (fn [n] (if (> n 0) (dec n) n))))

(defn- start []
  (reset! interval-id (js/setInterval #(swap! counter forward) @interval)))

(defn- stop []
  (js/clearInterval @interval-id)
  (reset! interval-id nil))

(defn- reset []
  (reset! counter 0))

(defn- running? []
  (not= nil @interval-id))

(defn settings []
  (let [temp-size (r/atom @size)
        temp-interval (r/atom @interval)]
    (fn []
      [:div {:style {:text-align "left"
                     :margin "10px"}}
       [rm/close-button]
       [:h3 "Settings"]
       [:form {:on-submit (fn [e]
                            (.preventDefault e)
                            (reset! size @temp-size)
                            (reset! interval @temp-interval)
                            (rm/close-modal!))}
        [:div
         [:label
          "Cell size (px) "
          [:input {:type "text"
                   :default-value (str @size)
                   :placeholder "cell size"
                   :on-change (fn [e] (reset! temp-size (-> e .-target .-value)))}]]]
        [:div
         [:label
          "Interval (ms) "
          [:input {:type "text"
                   :default-value (str @interval)
                   :placeholder "Interval"
                   :on-change (fn [e] (reset! temp-interval (-> e .-target .-value)))}]]]
        [:div
         [:input {:type "submit"
                  :value "Apply"}]]]])))

(defn canvas [w h s]
  (reset! steps s)
  [:div
   [:div
    [:canvas {:id "canvas"
              :role "canvas-role"
              :width (* @size w)
              :height (* @size h)
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
             :on-click #(rm/modal! [settings])}]]])

(comment
  (reset)
  (start)
  (stop)
  (.log js/console (clj->js @steps)))
