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

(defn canvas-visible? [component]
  (not= nil (.queryByRole component "canvas-role")))

(defn element-visible? [component element]
  (not= nil (.queryByText component element)))

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
