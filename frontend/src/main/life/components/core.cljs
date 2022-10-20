(ns life.components.core
  (:require [reagent.core :as r]))

(defonce expression (r/atom ""))
(defonce response (r/atom nil))

(defn- call-calculate! [caller]
  (caller
    "/calculate"
    {"expression" @expression}
    (fn [r]
      (reset! response r))))

(defn title []
  [:h1 "Life in parenthesis"])

(defn- input [caller]
  (let [expr (r/atom "")]
    (fn []
      [:form {:role "submit-form-role"
              :on-submit (fn [e]
                           (.preventDefault e)
                           (reset! expression @expr)
                           (when-not (empty? @expression) (call-calculate! caller)))}
       [:input {:type "text"
                :value @expr
                :placeholder "Enter your expression here"
                :on-change (fn [e]
                             (reset! expr (-> e .-target .-value)))}]])))

(defn- generate-output []
  (if (= nil @response)
    [:p "Waiting for response"]
    [:div
     [:p "Expression: " @expression]
     (if (= 200 (-> @response :status))
       [:p "Result: " (-> @response :body :message :result)]
       [:p "Something bad happened: " (-> @response :body :message)])]))

(defn- output []
  (if (empty? @expression)
    [:p "Waiting for input"]
    [generate-output]))

(defn page [caller]
  [:div
   [input caller]
   [output]])
