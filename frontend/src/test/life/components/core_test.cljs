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

(defn element-exists-and-matches [component element matcher]
  (is (= matcher
         (-> component
             (.getByText element)
             (.-innerHTML)))))

(defn element-does-not-exists [component element]
  (is (= nil (.queryByText component element))))

(defn page-content []
  [:div
   [component/input]
   [component/output]])

(deftest title-component
  (testing "A title is displayed"
    (with-mounted-component
      [component/title]
      (fn [component]
        (element-exists-and-matches component #"(?i)life" "Life in parenthesis")))))

(deftest input-component
  (testing "Field is empty on load"
    (with-mounted-component
      [component/input]
      (fn [component]
        (is (= ""
               (-> component
                   (.getByPlaceholderText #"expression")
                   (.-innerHTML)))))))

  (testing "Can input text"
    (with-mounted-component
      [page-content]
      (fn [component]
        (let [input (.getByPlaceholderText component #"expression here")]
          (.change rtl/fireEvent input (clj->js {:target {:value "123"}}))
          (.submit rtl/fireEvent (.getByRole component "submit-form-role"))
          (r/flush)
          (element-exists-and-matches component #"(?i)expression" "Expression: 123"))))))

(deftest output-component
  (testing "A waiting message is displayed"
    (with-mounted-component
      [component/output]
      (fn [component]
        (element-exists-and-matches component #"(?i)waiting" "Waiting for input")
        (element-does-not-exists component #"(?i)expression"))))

  (testing "The input expression is displayed"
    (reset! component/expression "1|2")
    (with-mounted-component
      [component/output]
      (fn [component]
        (element-exists-and-matches component #"(?i)expression" "Expression: 1|2")
        (element-does-not-exists component #"(?i)waiting")))))
