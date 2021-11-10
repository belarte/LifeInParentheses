(ns life.patterns-test
  (:require [clojure.test :refer [deftest testing is]]
            [life.patterns :as patterns]))

(deftest offset-pattern
  (testing "Offset glider"
    (let [expected #{[2 2] [3 3] [3 4] [1 4] [2 4]}]
      (is (= expected (patterns/offset patterns/glider [1 2])))))
  (testing "Offset block"
    (let [expected #{[3 3] [3 4] [4 3] [4 4]}]
      (is (= expected (patterns/offset patterns/block [3 3]))))))

(deftest flip-x
  (testing "Flipping glider on the x axis at origin"
    (let [expected #{[1 0] [0 1] [0 2] [1 2] [2 2]}]
      (is (= expected (patterns/flip-x patterns/glider)))))
  (testing "Flipping glider on the x axis anywhere"
    (let [expected #{[6 1] [6 2] [7 1] [7 2]}]
      (is (= expected (patterns/flip-x (patterns/offset patterns/block [3 1]) 2 7))))))
