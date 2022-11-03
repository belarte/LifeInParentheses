(ns life.components.core-test
  (:require [cljs.test :refer [deftest testing is use-fixtures]]
            [life.app :as app]
            [life.components.utils :refer [teardown
                                           with-mounted-component
                                           submit-expression
                                           mock-caller
                                           element-visible?]]))

(use-fixtures :each {:after teardown})

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
