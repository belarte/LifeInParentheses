(ns life.components.core-test
  (:require [cljs.test :refer [deftest testing is use-fixtures]]
            [reagent.core :as r]
            ["@testing-library/react" :as rtl]
            [life.components.core :as component]
            [life.app :as app]))

(defn- teardown []
  (rtl/cleanup)
  (reset! component/expression ""))

(use-fixtures :each {:after teardown})

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

(defn change-settings [component size interval]
  (let [input-size (.getByLabelText component #"(?i)cell size")
        input-interval (.getByLabelText component #"(?i)interval")]
    (.change rtl/fireEvent input-size (clj->js {:target {:value size}}))
    (.change rtl/fireEvent input-interval (clj->js {:target {:value interval}}))
    (.click rtl/fireEvent (.getByText component #"(?i)apply"))
    (r/flush)))

(defn open-settings [component]
  (let [button (.getByText component "Settings")]
    (.click rtl/fireEvent button)
    (r/flush)))

(defn canvas-visible? [component]
  (not= nil (.queryByRole component "canvas-role")))

(defn canvas-dimensions [component]
  (let [c (.getByRole component "canvas-role")]
    [(.-width c) (.-height c)]))

(defn element-visible? [component element]
  (not= nil (.queryByText component element)))

(defn element-in-form?
  ([component element]
   (not= nil (.queryByLabelText component element)))
  ([component element value]
   (= value (-> component
                (.getByLabelText element)
                (.-value)))))

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

(deftest title-component
  (testing "A title is displayed"
    (with-mounted-component
      [app/app mock-caller]
      (fn [component]
        (is (element-visible? component "Life in parenthesis"))))))

(deftest page-component
  (testing "Field is empty on load and a waiting message is displayed"
    (with-mounted-component
      [app/app mock-caller]
      (fn [component]
        (is (= ""
               (-> component
                   (.getByPlaceholderText #"expression")
                   (.-innerHTML))))
        (is (element-visible? component "Waiting for input"))
        (is (not (element-visible? component #"(?i)expression")))
        (is (not (element-visible? component #"(?i)result")))
        (is (not (element-visible? component #"(?i)something bad"))))))

  (testing "Can submit an expression"
    (with-mounted-component
      [app/app mock-caller]
      (fn [component]
        (submit-expression component "1|2")
        (is (element-visible? component "Expression: 1|2 = 3"))
        (is (not (element-visible? component #"(?i)something bad"))))))

  (testing "An error is reported if a malformed expression is submitted"
    (with-mounted-component
      [app/app mock-caller]
      (fn [component]
        (submit-expression component "1|")
        (is (element-visible? component "Something bad happened: Malformed expression: 1|"))
        (is (not (element-visible? component #"(?i)result"))))))

  (testing "No waiting message is visible after succesfully submitting an expression"
    (with-mounted-component
      [app/app mock-caller]
      (fn [component]
        (submit-expression component "1|2")
        (is (not (element-visible? component #"(?i)waiting")))))))

(deftest canvas-component
  (with-mounted-component
    [app/app mock-caller]
    (fn [component]
      (testing "Before an expression is submitted"
        (testing "the canvas is not visible"
          (is (not (canvas-visible? component))))
        (testing "the buttons are not visible"
          (is (not (element-visible? component "Prev")))
          (is (not (element-visible? component "Start")))
          (is (not (element-visible? component "Next")))
          (is (not (element-visible? component "Reset")))))

      (testing "With a valid expression"
        (submit-expression component "1|2")
        (testing "the canvas is visible"
          (is (canvas-visible? component)))
        (testing "the buttons are visible"
          (is (element-visible? component "Prev"))
          (is (element-visible? component "Start"))
          (is (element-visible? component "Next"))
          (is (element-visible? component "Reset"))))

      (testing "With a malformed expression"
        (submit-expression component "1|")
        (testing "the canvas is not visible"
          (is (not (canvas-visible? component))))
        (testing "the buttons are not visible"
          (is (not (element-visible? component "Prev")))
          (is (not (element-visible? component "Start")))
          (is (not (element-visible? component "Next")))
          (is (not (element-visible? component "Reset")))))

      (testing "With an empty expression"
        (submit-expression component "")
        (testing "the canvas is not visible"
          (is (not (canvas-visible? component))))
        (testing "the buttons are not visible"
          (is (not (element-visible? component "Prev")))
          (is (not (element-visible? component "Start")))
          (is (not (element-visible? component "Next")))
          (is (not (element-visible? component "Reset"))))))))

(deftest settings-modal
  (with-mounted-component
    [app/app mock-caller]
    (fn [component]
      (submit-expression component "42 & 86")

      (testing "Options are not visible on main page"
        (is (not (element-in-form? component #"(?i)cell size")))
        (is (not (element-in-form? component #"(?i)interval")))
        (is (not (element-visible? component "Apply"))))

      (testing "Options are visible after opening settings"
        (open-settings component)
        (is (element-in-form? component #"(?i)cell size" "5"))
        (is (element-in-form? component #"(?i)interval" "1000"))
        (is (element-visible? component "Apply")))

      (testing "Before changing settings the canvas has correct dimensions"
        (is (= [(* 136 5) (* 17 5)] (canvas-dimensions component)))
        (change-settings component "10" "100")
        (is (= [(* 136 10) (* 17 10)] (canvas-dimensions component))))

      (testing "Options are no longer visible after settings are updated"
        (is (not (element-in-form? component #"(?i)cell size")))
        (is (not (element-in-form? component #"(?i)interval")))
        (is (not (element-visible? component "Apply"))))

      (testing "Settings have been updated"
        (open-settings component)
        (is (element-in-form? component #"(?i)cell size" "10"))
        (is (element-in-form? component #"(?i)interval" "100"))))))
