(ns life.life-test
  (:require [clojure.test :refer [deftest testing is]]
            [life.life :as life]
            [life.patterns :as patterns]))

(def stepper life/conway-stepper)

(def glider-board (life/create-board 4 4 [patterns/glider]))
(def block-board (life/create-board 2 2 [patterns/block]))
(def eater-board (life/create-board 4 4 [patterns/eater]))

(deftest life-is-evolving-one-step-at-a-time
  (testing "Still life remains the same"
    (is (= block-board (stepper block-board)))
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
    (let [output (last (life/simulate glider-board 4))
          expected (life/create-board 4 4 [(patterns/offset patterns/glider [1 1])])]
      (is (= expected output)))))

(deftest glider-is-eaten
  (testing "Glider is eaten by eater"
    (let [glider patterns/glider
          eater (patterns/offset patterns/eater [5 6])
          board (life/create-board 10 10 [glider eater])
          pattern (:alive-cells (last (life/simulate board 12)))]
      (is (= eater pattern)))))
