(ns life.app
  (:require [reagent.core :as r]
            [reagent.dom :as d]
            [life.components.core :as c]
            [life.network.http :as h]))

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
   [c/input]
   [c/output]])

(defn app []
  (check-server!)
  [:div
   [c/title]
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
