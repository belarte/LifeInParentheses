(ns alu.layout-test
  (:require [clojure.test :refer [deftest testing is]]
            [alu.layout :as layout]))

(def expression {:dimensions {:origin [1 1]
                              :width 6
                              :height 4}
                 :output {:position [4 4]
                          :direction :bottom-right}
                 :steps 16
                 :pattern #{[1 1] [2 3] [6 4]}})

(deftest flip-x
  (testing "Flipping twice returns copy of input"
    (is (= expression (layout/flip-x (layout/flip-x expression)))))
  (testing "Can flip an expression on the X axis"
    (let [result (layout/flip-x expression)
          {:keys [dimensions output steps pattern]} result]
      (is (= [1 1] (dimensions :origin)))
      (is (= 6 (dimensions :width)))
      (is (= 4 (dimensions :height)))
      (is (= [3 4] (output :position)))
      (is (= :bottom-left (output :direction)))
      (is (= 16 steps))
      (is (= #{[6 1] [5 3] [1 4]} pattern)))))

(deftest align-for-intesection
  (let [left {:dimensions {:origin [2 2]
                           :width 6
                           :height 5}
              :output {:position [4 5]
                       :direction :bottom-left}
              :steps 24}
        right {:dimensions {:origin [4 1]
                            :width 4
                            :height 4}
               :output {:position [6 3]
                        :direction :bottom-right}
               :steps 16}
        [left-output right-output] (layout/align-for-intersection left right)]
    (testing "Result expressions do not overlap"
      (let [[xl] (get-in left-output [:dimensions :origin])
            [xr] (get-in right-output [:dimensions :origin])
            w (get-in left-output [:dimensions :width])]
        (is (>= xr (+ xl w)))))
    (testing "X origin coordinates are separated by an even number of cells"
      (let [[xl] (get-in left-output [:output :position])
            [xr] (get-in right-output [:output :position])]
        (is (odd? (- xr xl)))))
    (testing "Both result expressiona have same number of steps"
      (let [left-steps (left-output :steps)
            right-steps (right-output :steps)]
        (is (= left-steps right-steps))))
    (testing "Outputs align"
      (let [[_ yl] (get-in left-output [:output :position])
            [_ yr] (get-in right-output [:output :position])]
        (is (= yr (- yl 1)))))
    (testing "Throws exception if outputs have the same direction"
      (is (thrown? AssertionError (layout/align-for-intersection left left)))
      (is (thrown? AssertionError (layout/align-for-intersection right right))))
    (testing "Organise epressions so first is going bottom-right and second bottom-left"
      (is (= :bottom-right (get-in left-output [:output :direction])))
      (is (= :bottom-left (get-in right-output [:output :direction]))))))
