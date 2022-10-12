(ns life.app
  (:require [reagent.core :as r]
            [reagent.dom :as d]))

(defonce expression (r/atom ""))

(defn input-component []
  (let [expr (r/atom "")]
    (fn []
      [:form {:on-submit (fn [e]
                           (.preventDefault e)
                           (reset! expression @expr))}
       [:input {:type "text"
                :value @expr
                :placeholder "Enter your expression here"
                :on-change (fn [e]
                             (reset! expr (-> e .-target .-value)))}]])))

(defn output-component []
  (if (empty? @expression)
    [:p "Waiting for input"]
    [:p "Expression:" @expression]))

(defn app []
  [:div
   [:h1 "Life in parenthesis"]
   [input-component]
   [output-component]])

(defn ^:export init []
  (d/render [app] (js/document.getElementById "root")))

(comment
  (init)
  (js/alert "Run this form to check connection with nREPL"))
