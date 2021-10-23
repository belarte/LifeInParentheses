(ns life-test
  (:require [clojure.test :refer [deftest testing is]]
            [life]))

(deftest life-is-evolving
  (testing "Still life remains the same"
    (let [simulator (life/create)]
      (is (= life/pattern-square (simulator life/pattern-square)))
      (is (= life/pattern-eater (simulator life/pattern-eater))))))
