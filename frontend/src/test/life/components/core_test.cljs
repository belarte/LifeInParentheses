(ns life.components.core-test
  (:require [cljs.test :refer [deftest testing is use-fixtures]]
            [reagent.core :as r]
            ["@testing-library/react" :as rtl]
            [life.components.core :as component]))

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

(defn canvas-visible? [component]
  (is (not= nil (.queryByRole component "canvas-role"))))

(defn canvas-not-visible? [component]
  (is (= nil (.queryByRole component "canvas-role"))))

(defn submit-expression [component expression]
  (let [input (.getByPlaceholderText component #"expression here")]
    (.change rtl/fireEvent input (clj->js {:target {:value expression}}))
    (.submit rtl/fireEvent (.getByRole component "submit-form-role"))
    (r/flush)))

(defn element-exists [component matcher]
  (is (= matcher
         (-> component
             (.getByText matcher)
             (.-innerHTML)))))

(defn element-does-not-exists [component element]
  (is (= nil (.queryByText component element))))

(defn mock-caller> [responses]
  (fn [_ params callback]
    (let [param (get params "expression")
          resp  (get responses param)]
      (callback resp))))

(def responses {"1|2" {:status 200
                       :body {:message {:result 3}}}
                "1|" {:status 400
                      :body {:message "Malformed expression: 1|"}}})

(def mock-caller (mock-caller> responses))

(deftest title-component
  (testing "A title is displayed"
    (with-mounted-component
      [component/title]
      (fn [component]
        (element-exists component "Life in parenthesis")))))

(deftest page-component
  (testing "Field is empty on load and a waiting message is displayed"
    (with-mounted-component
      [component/page nil]
      (fn [component]
        (is (= ""
               (-> component
                   (.getByPlaceholderText #"expression")
                   (.-innerHTML))))
        (element-exists component "Waiting for input")
        (element-does-not-exists component #"(?i)expression")
        (element-does-not-exists component #"(?i)result")
        (element-does-not-exists component #"(?i)something bad")
        (canvas-not-visible? component))))

  (testing "Can submit an expression"
    (with-mounted-component
      [component/page mock-caller]
      (fn [component]
        (submit-expression component "1|2")
        (element-exists component "Expression: 1|2")
        (element-exists component "Result: 3")
        (canvas-visible? component)
        (element-does-not-exists component #"(?i)waiting")
        (element-does-not-exists component #"(?i)something bad"))))

  (testing "An error is reported if a malformed expression is submitted"
    (with-mounted-component
      [component/page mock-caller]
      (fn [component]
        (submit-expression component "1|")
        (element-exists component "Expression: 1|")
        (element-exists component "Something bad happened: Malformed expression: 1|")
        (canvas-not-visible? component)
        (element-does-not-exists component #"(?i)waiting")
        (element-does-not-exists component #"(?i)result")))))
