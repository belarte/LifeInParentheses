(ns life.components.core
  (:require [reagent.core :as r]))

(defonce expression (r/atom ""))

(defn title []
  [:h1 "Life in parenthesis"])

(defn input []
  (let [expr (r/atom "")]
    (fn []
      [:form {:role "submit-form-role"
              :on-submit (fn [e]
                           (.preventDefault e)
                           (reset! expression @expr))}
       [:input {:type "text"
                :value @expr
                :placeholder "Enter your expression here"
                :on-change (fn [e]
                             (reset! expr (-> e .-target .-value)))}]])))

(defn output []
  (if (empty? @expression)
    [:p "Waiting for input"]
    [:p "Expression: " @expression]))
