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
        (is (element-visible? component "Life in parenthesis"))))))

(deftest page-component
  (testing "Field is empty on load and a waiting message is displayed"
    (with-mounted-component
      [component/page nil]
      (fn [component]
        (is (= ""
               (-> component
                   (.getByPlaceholderText #"expression")
                   (.-innerHTML))))
        (is (element-visible? component "Waiting for input"))
        (is (not (element-visible? component #"(?i)expression")))
        (is (not (element-visible? component #"(?i)result")))
        (is (not (element-visible? component #"(?i)something bad")))
        (is (not (canvas-visible? component))))))

  (testing "Can submit an expression"
    (with-mounted-component
      [component/page mock-caller]
      (fn [component]
        (submit-expression component "1|2")
        (is (element-visible? component "Expression: 1|2 = 3"))
        (is (canvas-visible? component))
        (is (not (element-visible? component #"(?i)something bad"))))))

  (testing "An error is reported if a malformed expression is submitted"
    (with-mounted-component
      [component/page mock-caller]
      (fn [component]
        (submit-expression component "1|")
        (is (element-visible? component "Something bad happened: Malformed expression: 1|"))
        (is (not (canvas-visible? component)))
        (is (not (element-visible? component #"(?i)result"))))))

  (testing "No waiting message is visible after succesfully submitting an expression"
    (with-mounted-component
      [component/page mock-caller]
      (fn [component]
        (submit-expression component "1|2")
        (is (not (element-visible? component #"(?i)waiting")))))))
