(ns life.life-test
  (:require [clojure.test :refer [deftest testing is]]
            [life.life :as life]))

(def stepper life/conway-stepper)

(def glider (life/patterns :glider))
(def square (life/patterns :square))
(def eater (life/patterns :eater))

(def glider-board (life/create-board 4 4 [glider]))
(def square-board (life/create-board 2 2 [square]))
(def eater-board (life/create-board 9 6 [eater]))

(deftest life-is-evolving-one-step-at-a-time
  (testing "Still life remains the same"
    (is (= square-board (stepper square-board)))
    (is (= eater-board (stepper eater-board))))
  (testing "Glider evolves as expected"
    (let [expected (life/create-board 4 4 [#{[0 1] [2 1] [1 2] [2 2] [1 3]}])]
      (is (= expected (stepper glider-board)))))
  (testing "Blinker evolves as expected"
    (let [blinker (life/create-board 3 3 [#{[0 1] [1 1] [2 1]}])
          expected (life/create-board 3 3 [#{[1 0] [1 1] [1 2]}])]
      (is (= expected (stepper blinker)))
      (is (= blinker (stepper (stepper blinker)))))))

(deftest life-is-evolving
  (testing "Glider keeps its shape after 4 steps"
    (let [expected (life/create-board 4 4 [(life/offset glider [1 1])])]
      (is (= expected (life/simulate glider-board 4))))))

(deftest offset-pattern
  (testing "Offset glider"
    (let [expected #{[2 2] [3 3] [3 4] [1 4] [2 4]}]
      (is (= expected (life/offset glider [1 2])))))
  (testing "Offset square"
    (let [expected #{[3 3] [3 4] [4 3] [4 4]}]
      (is (= expected (life/offset square [3 3]))))))

(deftest flip-x
  (testing "Flipping glider on the x axis"
    (let [expected #{[1 0] [0 1] [0 2] [1 2] [2 2]}]
      (is (= expected (life/flip-x glider))))))
