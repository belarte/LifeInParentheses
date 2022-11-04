(ns life.components.settings
  (:require [reagent.core :as r]
            [reagent-mui.material.button :refer [button]]
            [reagent-mui.material.text-field :refer [text-field]]
            [reagent-mui.material.dialog :refer [dialog]]
            [reagent-mui.material.dialog-title :refer [dialog-title]]
            [reagent-mui.material.dialog-content :refer [dialog-content]]
            [reagent-mui.material.dialog-actions :refer [dialog-actions]]))

(defonce size (r/atom 5))
(defonce interval (r/atom 1000))

(defonce open (r/atom false))

(defn settings []
  (let [temp-size (r/atom @size)
        temp-interval (r/atom @interval)]
    [dialog {:open @open
             :on-close (fn []
                         (reset! open false))}
     [dialog-title "Settings"]
     [dialog-content
      [text-field {:label "Cell size (px)"
                   :auto-focus true
                   :margin "dense"
                   :full-width true
                   :type "number"
                   :default-value (str @size)
                   :variant "standard"
                   :on-change (fn [e] (reset! temp-size (-> e .-target .-value)))}]
      [text-field {:label "Interval (ms)"
                   :auto-focus true
                   :margin "dense"
                   :full-width true
                   :type "number"
                   :default-value (str @interval)
                   :variant "standard"
                   :on-change (fn [e] (reset! temp-interval (-> e .-target .-value)))}]]
     [dialog-actions
      [button {:on-click #(reset! open false)} "Close"]
      [button {:on-click (fn []
                           (reset! size @temp-size)
                           (reset! interval @temp-interval)
                           (reset! open false))}
       "Apply"]]]))
