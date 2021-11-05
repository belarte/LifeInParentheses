(ns life.coords-test
  (:require [clojure.test :refer [deftest testing is]]
            [life.coords :as c]))

(deftest add
  (testing "Adding positive values"
    (is (= [2 2] (c/add [0 0] [2 2])))
    (is (= [2 2] (c/add [1 1] [1 1])))
    (is (= [2 2] (c/add [2 2] [0 0]))))
  (testing "Adding negative values"
    (is (= [0 0] (c/add [2 2] [-2 -2])))
    (is (= [0 0] (c/add [-2 -2] [2 2])))
    (is (= [-3 -3] (c/add [-1 -2] [-2 -1])))))

(deftest sub
  (testing "Substracting positive values"
    (is (= [-2 -2] (c/sub [0 0] [2 2])))
    (is (= [0 0] (c/sub [1 1] [1 1])))
    (is (= [2 2] (c/sub [2 2] [0 0]))))
  (testing "Substracting negative values"
    (is (= [4 4] (c/sub [2 2] [-2 -2])))
    (is (= [-4 -4] (c/sub [-2 -2] [2 2])))
    (is (= [1 -1] (c/sub [-1 -2] [-2 -1])))))

(deftest neighbours
  (testing "Neighbours at origin"
    (is (= #{[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]} (set (c/neighbours [0 0])))))
  (testing "Neighbours at positive coords"
    (is (= #{[4 4] [4 5] [4 6] [5 4] [5 6] [6 4] [6 5] [6 6]} (set (c/neighbours [5 5]))))))
