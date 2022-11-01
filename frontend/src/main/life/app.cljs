(ns life.app
  (:require [reagent.core :as r]
            [reagent.dom :as d]
            [reagent-modals.modals :as rm]
            [life.components.core :as c]
            [life.network.http :as h]))

(defonce server-available? (r/atom false))
(defonce error-message (r/atom ""))

(defn check-server! [caller]
  (caller
    "/health"
    (fn [response]
      (reset! server-available? (= (response :status) 200))
      (reset! error-message (response :error-text)))))

(def app-style
  {:text-align "center"
   :border "2px solid blue"})

(defn app [caller]
  (check-server! caller)
  [:div {:style app-style}
   [c/title]
   (if @server-available?
     [c/page caller]
     [:p "Server is not available: " @error-message])
   [rm/modal-window]])

(defn ^:dev/after-load render []
  (d/render [app h/call] (js/document.getElementById "root")))

(defn ^:export init []
  (render))

(comment
  (init)
  (js/alert "Run this form to check connection with nREPL"))
