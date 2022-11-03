(ns life.components.core-test
  (:require [cljs.test :refer [deftest testing is use-fixtures]]
            [life.app :as app]
            [life.components.utils :refer [teardown
                                           with-mounted-component
                                           submit-expression
                                           mock-caller
                                           element-visible?
                                           canvas-visible?]]))

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
