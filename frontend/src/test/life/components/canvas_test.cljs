(ns life.components.canvas-test
  (:require [cljs.test :refer [deftest testing is use-fixtures]]
            [life.app :as app]
            [life.components.utils :refer [teardown
                                           with-mounted-component
                                           submit-expression
                                           mock-caller
                                           element-visible?
                                           canvas-visible?
                                           button-visible?]]))

(use-fixtures :each {:after teardown})

(deftest before-expression-submitted
  (testing "Before an expression is submitted"
    (with-mounted-component
      [app/app mock-caller]
      (fn [component]
          (testing "the canvas is not visible"
            (is (not (canvas-visible? component))))
          (testing "the buttons are not visible"
            (is (not (element-visible? component "Prev")))
            (is (not (element-visible? component "Start")))
            (is (not (element-visible? component "Next")))
            (is (not (element-visible? component "Reset"))))))))

(deftest after-expression-submitted
  (with-mounted-component
    [app/app mock-caller]
    (fn [component]
      (testing "With a valid expression"
        (submit-expression component "1|2")
        (testing "the canvas is visible"
          (is (canvas-visible? component)))
        (testing "the buttons are visible"
          (is (button-visible? component "back-button"))
          (is (button-visible? component "play-button"))
          (is (button-visible? component "next-button"))
          (is (button-visible? component "reset-button"))))

      (testing "With a malformed expression"
        (submit-expression component "1|")
        (testing "the canvas is not visible"
          (is (not (canvas-visible? component))))
        (testing "the buttons are not visible"
          (is (not (button-visible? component "back-button")))
          (is (not (button-visible? component "play-button")))
          (is (not (button-visible? component "next-button")))
          (is (not (button-visible? component "reset-button")))))

      (testing "With an empty expression"
        (submit-expression component "")
        (testing "the canvas is not visible"
          (is (not (canvas-visible? component))))
        (testing "the buttons are not visible"
          (is (not (button-visible? component "back-button")))
          (is (not (button-visible? component "play-button")))
          (is (not (button-visible? component "next-button")))
          (is (not (button-visible? component "reset-button"))))))))
