(ns life.components.settings
  (:require [reagent.core :as r]
            [reagent-modals.modals :as rm]))

(defonce size (r/atom 5))
(defonce interval (r/atom 1000))

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
