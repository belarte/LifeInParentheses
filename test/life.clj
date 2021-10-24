(ns life-test
  (:require [clojure.test :refer [deftest testing is]]
            [life]))

(deftest life-is-evolving
  (testing "Still life remains the same"
    (let [stepper life/conway-stepper]
      (is (= life/pattern-square (stepper life/pattern-square)))
      (is (= life/pattern-eater (stepper life/pattern-eater)))))
  (testing "Glider evolves as expected"
    (let [stepper life/conway-stepper
          glider (life/create-world 4 4 [life/pattern-glider])
          after-one-step (life/create-world 4 4 [{:width 4
                                                  :height 4
                                                  :alive-cells #{[0 1] [2 1] [1 2] [2 2] [1 3]}}])]
      (is (= after-one-step (stepper glider)))))
  (testing "Blinker evolves as expected"
    (let [stepper life/conway-stepper
          blinker (life/create-world 3 3 [{:width 3
                                           :height 1
                                           :alive-cells #{[0 1] [1 1] [2 1]}}])
          after-one-step (life/create-world 3 3 [{:width 3
                                                  :height 3
                                                  :alive-cells #{[1 0] [1 1] [1 2]}}])]
      (is (= after-one-step (stepper blinker)))
      (is (= blinker (stepper (stepper blinker)))))))
