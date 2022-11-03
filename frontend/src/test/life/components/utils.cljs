(ns life.components.utils
  (:require [reagent.core :as r]
            ["@testing-library/react" :as rtl]
            [life.components.core :as c]))

(defn teardown []
  (rtl/cleanup)
  (reset! c/expression ""))

(defn with-mounted-component [component f]
  (let [mounted-component (rtl/render (r/as-element component))]
    (try
      (f mounted-component)
      (finally
        (.unmount mounted-component)
        (r/flush)))))

(defn submit-expression [component expression]
  (let [input (.getByPlaceholderText component #"expression here")]
    (.change rtl/fireEvent input (clj->js {:target {:value expression}}))
    (.submit rtl/fireEvent (.getByRole component "submit-form-role"))
    (r/flush)))

(defn element-visible? [component element]
  (not= nil (.queryByText component element)))

(defn canvas-visible? [component]
  (not= nil (.queryByRole component "canvas-role")))

(defn canvas-dimensions [component]
  (let [c (.getByRole component "canvas-role")]
    [(.-width c) (.-height c)]))

(defn element-in-form?
  ([component element]
   (not= nil (.queryByLabelText component element)))
  ([component element value]
   (= value (-> component
                (.getByLabelText element)
                (.-value)))))

(defn open-settings [component]
  (let [button (.getByRole component "button" (clj->js {:name "Settings"}))]
    (.click rtl/fireEvent button)
    (r/flush)))

(defn change-settings [component size interval]
  (let [input-size (.getByLabelText component #"(?i)cell size")
        input-interval (.getByLabelText component #"(?i)interval")]
    (.change rtl/fireEvent input-size (clj->js {:target {:value size}}))
    (.change rtl/fireEvent input-interval (clj->js {:target {:value interval}}))
    (.click rtl/fireEvent (.getByText component #"(?i)apply"))
    (r/flush)))

(defn mock-caller> [responses]
  (fn
    ([_ callback]
     (callback {:status 200
                :error-text "No error"}))
    ([_ params callback]
     (let [param (get params "expression")
           resp  (get responses param)]
       (callback resp)))))

(def responses {"1|2" {:status 200
                       :body {:message {:result 3}}}
                "42 & 86" {:status 200
                           :body {:message {:result 2
                                            :width 136
                                            :height 17}}}
                "1|" {:status 400
                      :body {:message "Malformed expression: 1|"}}})

(def mock-caller (mock-caller> responses))
