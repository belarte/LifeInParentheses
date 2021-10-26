(ns life.life-test
  (:require [clojure.test :refer [deftest testing is]]
            [life.life :as life]))

(deftest life-is-evolving
  (testing "Still life remains the same"
    (let [stepper life/conway-stepper
          eater (life/create-board 9 6 [(life/patterns :eater)])
          square (life/create-board 2 2 [(life/patterns :square)])]
      (is (= square (stepper square)))
      (is (= eater (stepper eater)))))
  (testing "Glider evolves as expected"
    (let [stepper life/conway-stepper
          glider (life/create-board 4 4 [(life/patterns :glider)])
          after-one-step (life/create-board 4 4 [#{[0 1] [2 1] [1 2] [2 2] [1 3]}])]
      (is (= after-one-step (stepper glider)))))
  (testing "Blinker evolves as expected"
    (let [stepper life/conway-stepper
          blinker (life/create-board 3 3 [#{[0 1] [1 1] [2 1]}])
          after-one-step (life/create-board 3 3 [#{[1 0] [1 1] [1 2]}])]
      (is (= after-one-step (stepper blinker)))
      (is (= blinker (stepper (stepper blinker)))))))

(deftest offset-pattern
  (testing "Offset glider"
    (is (= (life/offset (life/patterns :glider) [1 2]) #{[2 2] [3 3] [3 4] [1 4] [2 4]})))
  (testing "Offset square"
    (is (= (life/offset (life/patterns :square) [3 3]) #{[3 3] [3 4] [4 3] [4 4]}))))
