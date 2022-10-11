(ns life.app
  (:require [reagent.dom :as d]))

(defn app []
  [:div
   [:h1 "Life in parenthesis"]
   [:p "(incoming soon...)"]])

(defn ^:export init []
  (d/render [app] (js/document.getElementById "root")))

(comment
  (init)
  (js/alert "Run this form to check connection with nREPL"))
