(ns life.app
  (:require [reagent.core :as r]
            [reagent.dom :as d]
            [life.network.http :as h]))

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

(defonce server-available? (r/atom false))
(defonce error-message (r/atom ""))

(defn check-server! []
  (h/call
    "/health"
    (fn [response]
      (reset! server-available? (= (response :status) 200))
      (reset! error-message (response :error-text)))))

(defn page-content []
  [:div
   [input-component]
   [output-component]])

(defn app []
  (check-server!)
  [:div
   [:h1 "Life in parenthesis"]
   (if @server-available?
     [page-content]
     [:p "Server is not available: " @error-message])])

(defn ^:dev/after-load render []
  (d/render [app] (js/document.getElementById "root")))

(defn ^:export init []
  (render))

(comment
  (init)
  (js/alert "Run this form to check connection with nREPL"))
