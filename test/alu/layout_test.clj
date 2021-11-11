(ns alu.layout-test
  (:require [clojure.test :refer [deftest testing is]]
            [alu.layout :as layout]))

(deftest flip-x
  (testing "Can flip an expression on the X axis"
    (let [expression {:dimensions {:origin [1 1]
                                   :width 6
                                   :height 4}
                      :output {:position [4 4]
                               :direction :bottom-right}
                      :steps 16
                      :pattern #{[1 1] [2 3] [6 4]}}
          result (layout/flip-x expression)
          {:keys [dimensions output steps pattern]} result]
      (is (= [1 1] (dimensions :origin)))
      (is (= 6 (dimensions :width)))
      (is (= 4 (dimensions :height)))
      (is (= [3 4] (output :position)))
      (is (= :bottom-left (output :direction)))
      (is (= 16 steps))
      (is (= #{[6 1] [5 3] [1 4]} pattern)))))
