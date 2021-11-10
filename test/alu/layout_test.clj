(ns alu.layout-test
  (:require [clojure.test :refer [deftest testing is]]
            [alu.layout :as layout]))

(deftest flip-x
  (testing "Can flip an expression on the X axis"
    (let [expression {:dimension {:origin [1 1]
                                  :width 6
                                  :height 4}
                      :output {:position [4 4]
                               :direction :bottom-right}
                      :steps 16
                      :pattern #{[1 1] [2 3] [6 4]}}
          result (layout/flip-x expression)
          {:keys [dimension output steps pattern]} result]
      (is (= [1 1] (dimension :origin)))
      (is (= 6 (dimension :width)))
      (is (= 4 (dimension :height)))
      (is (= [3 4] (output :position)))
      (is (= :bottom-left (output :direction)))
      (is (= 16 steps))
      (is (= #{[6 1] [5 3] [1 4]} pattern)))))
