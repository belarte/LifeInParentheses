(ns life.components.core
  (:require [reagent.core :as r]
            [life.components.canvas :refer [canvas]]))

(defonce expression (r/atom ""))
(defonce response (r/atom nil))

(defn- call-calculate! [caller]
  (caller
    "/calculate"
    {"expression" @expression
     "steps" true}
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
     (let [message (-> @response :body :message)]
       (if (= 200 (-> @response :status))
         (let [w (message :width)
               h (message :height)
               s (message :steps)]
           [:div
            [:p "Result: " (message :result)]
            [canvas w h s]])
         [:p "Something bad happened: " message]))]))

(defn- output []
  (if (empty? @expression)
    [:p "Waiting for input"]
    [generate-output]))

(defn page [caller]
  [:div
   [input caller]
   [output]])

(comment
  (do
    (require '[life.network.http :as h])
    (reset! expression "127 | 128")
    (call-calculate! h/call))
  (.log js/console (clj->js @response)))
