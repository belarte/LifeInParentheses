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
