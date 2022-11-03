(ns life.components.settings-test
  (:require [cljs.test :refer [deftest testing is use-fixtures]]
            [life.app :as app]
            [life.components.utils :refer [teardown
                                           with-mounted-component
                                           submit-expression
                                           mock-caller
                                           element-in-form?
                                           element-visible?
                                           canvas-dimensions
                                           change-settings
                                           open-settings]]))

(use-fixtures :each {:after teardown})

(deftest before-opening-settings
  (testing "Before opening settings,"
    (with-mounted-component
      [app/app mock-caller]
      (fn [component]
        (submit-expression component "42 & 86")

        (testing "settings are not visible"
          (is (not (element-in-form? component #"(?i)cell size")))
          (is (not (element-in-form? component #"(?i)interval")))
          (is (not (element-visible? component "Apply"))))

        (testing "the canvas has correct dimensions"
          (is (= [(* 136 5) (* 17 5)] (canvas-dimensions component))))))))

(deftest settings-opened
  (testing "When settings are opened"
    (with-mounted-component
      [app/app mock-caller]
      (fn [component]
        (submit-expression component "42 & 86")

        (testing "settings are visible"
          (open-settings component)
          (is (element-in-form? component #"(?i)cell size" "5"))
          (is (element-in-form? component #"(?i)interval" "1000"))
          (is (element-visible? component "Apply")))))))

(deftest after-settings-update
  (testing "After updating the settings,"
    (with-mounted-component
      [app/app mock-caller]
      (fn [component]
        (submit-expression component "42 & 86")
        (open-settings component)
        (change-settings component "10" "100")

        (testing "settings are no longer visible"
          (is (not (element-in-form? component #"(?i)cell size")))
          (is (not (element-in-form? component #"(?i)interval")))
          (is (not (element-visible? component "Apply"))))

        (testing "the canvas has correct dimensions"
          (is (= [(* 136 10) (* 17 10)] (canvas-dimensions component))))

        (testing "settings have been updated"
          (open-settings component)
          (is (element-in-form? component #"(?i)cell size" "10"))
          (is (element-in-form? component #"(?i)interval" "100")))))))
