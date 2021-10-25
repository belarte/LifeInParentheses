(ns life-test
  (:require [clojure.test :refer [deftest testing is]]
            [life]))

(deftest life-is-evolving
  (testing "Still life remains the same"
    (let [stepper life/conway-stepper
          eater (life/create-world 9 6 [(life/patterns :eater)])
          square (life/create-world 2 2 [(life/patterns :square)])]
      (is (= square (stepper square)))
      (is (= eater (stepper eater)))))
  (testing "Glider evolves as expected"
    (let [stepper life/conway-stepper
          glider (life/create-world 4 4 [(life/patterns :glider)])
          after-one-step (life/create-world 4 4 [#{[0 1] [2 1] [1 2] [2 2] [1 3]}])]
      (is (= after-one-step (stepper glider)))))
  (testing "Blinker evolves as expected"
    (let [stepper life/conway-stepper
          blinker (life/create-world 3 3 [#{[0 1] [1 1] [2 1]}])
          after-one-step (life/create-world 3 3 [#{[1 0] [1 1] [1 2]}])]
      (is (= after-one-step (stepper blinker)))
      (is (= blinker (stepper (stepper blinker)))))))
