(ns alu.layout-test
  (:require [clojure.test :refer [deftest testing is are]]
            [alu.layout :as layout]))

(def expression {:alu/dimensions {:alu/origin [1 1]
                                  :alu/width 6
                                  :alu/height 4}
                 :alu/output {:alu/position [4 4]
                              :alu/direction :bottom-right}
                 :alu/steps 16
                 :alu/pattern #{[1 1] [2 3] [6 4]}})

(def left {:alu/dimensions {:alu/origin [2 2]
                            :alu/width 6
                            :alu/height 5}
           :alu/output {:alu/position [4 5]
                        :alu/direction :bottom-left}
           :alu/steps 24})
(def right {:alu/dimensions {:alu/origin [4 1]
                             :alu/width 4
                             :alu/height 4}
            :alu/output {:alu/position [6 3]
                         :alu/direction :bottom-right}
            :alu/steps 16})

(defn within-bounds-test-helper [pattern]
  {:alu/dimensions {:alu/origin [2 2]
                    :alu/width 5
                    :alu/height 4}
   :alu/pattern pattern})

(deftest within-bounds
  (testing "Expression is within the bounds"
    (is (true? (layout/within-bounds? (within-bounds-test-helper #{[2 2] [6 2] [5 2] [6 5]})))))
  (testing "Expression are not within the bounds"
    (are [pattern] (false? (layout/within-bounds? (within-bounds-test-helper pattern)))
         #{[1 2]}
         #{[2 1]}
         #{[7 5]}
         #{[6 6]})))

(deftest flip-x
  (testing "Flipping twice returns copy of input"
    (is (= expression (layout/flip-x (layout/flip-x expression)))))
  (testing "Can flip an expression on the X axis"
    (let [result (layout/flip-x expression)
          {:keys [alu/dimensions alu/output alu/steps alu/pattern]} result]
      (is (= [1 1] (dimensions :alu/origin)))
      (is (= 6 (dimensions :alu/width)))
      (is (= 4 (dimensions :alu/height)))
      (is (= [3 4] (output :alu/position)))
      (is (= :bottom-left (output :alu/direction)))
      (is (= 16 steps))
      (is (= #{[6 1] [5 3] [1 4]} pattern)))))

(deftest wire
  (testing "Check left facing expression"
    (let [output (layout/wire left 3)]
      (is (= [2 2] (get-in output [:alu/dimensions :alu/origin])))
      (is (= 8 (get-in output [:alu/dimensions :alu/width])))
      (is (= 8 (get-in output [:alu/dimensions :alu/height])))
      (is (= [3 8] (get-in output [:alu/output :alu/position])))
      (is (= :bottom-left (get-in output [:alu/output :alu/direction])))
      (is (= 36 (output :alu/steps)))))
  (testing "Check right facing expression"
    (let [output (layout/wire right 3)]
      (is (= [4 1] (get-in output [:alu/dimensions :alu/origin])))
      (is (= 7 (get-in output [:alu/dimensions :alu/width])))
      (is (= 7 (get-in output [:alu/dimensions :alu/height])))
      (is (= [9 6] (get-in output [:alu/output :alu/position])))
      (is (= :bottom-right (get-in output [:alu/output :alu/direction])))
      (is (= 28 (output :alu/steps))))))

(deftest shift
  (let [e {:alu/dimensions {:alu/origin [1 2]}
           :alu/output {:alu/position [5 6]}
           :alu/pattern #{[1 2] [2 3] [3 4] [4 5]}}
        shifted (layout/shift e [1 2])]
    (testing "Check shifting expression"
      (is (= [2 4] (get-in shifted [:alu/dimensions :alu/origin])))
      (is (= [6 8] (get-in shifted [:alu/output :alu/position])))
      (is (= #{[2 4] [3 5] [4 6] [5 7]} (shifted :alu/pattern))))))

(deftest align-for-intesection
  (let [[left-output right-output] (layout/align-for-intersection left right)]
    (testing "Result expressions do not overlap"
      (let [[xl] (get-in left-output [:alu/dimensions :alu/origin])
            [xr] (get-in right-output [:alu/dimensions :alu/origin])
            w (get-in left-output [:alu/dimensions :alu/width])]
        (is (>= xr (+ xl w)))))
    (testing "X origin coordinates are separated by an odd number of cells"
      (let [[xl] (get-in left-output [:alu/output :alu/position])
            [xr] (get-in right-output [:alu/output :alu/position])]
        (is (odd? (- xr xl 1)))))
    (testing "Both result expressiona have same number of steps"
      (let [left-steps (left-output :alu/steps)
            right-steps (right-output :alu/steps)]
        (is (= left-steps right-steps))))
    (testing "Outputs align"
      (let [[_ yl] (get-in left-output [:alu/output :alu/position])
            [_ yr] (get-in right-output [:alu/output :alu/position])]
        (is (= yr (- yl 1)))))
    (testing "Throws exception if outputs have the same direction"
      (is (thrown? AssertionError (layout/align-for-intersection left left)))
      (is (thrown? AssertionError (layout/align-for-intersection right right))))
    (testing "Organise epressions so first is going bottom-right and second bottom-left"
      (is (= :bottom-right (get-in left-output [:alu/output :alu/direction])))
      (is (= :bottom-left (get-in right-output [:alu/output :alu/direction]))))))

(deftest merge-expressions
  (let [left {:alu/dimensions {:alu/origin [1 2]
                               :alu/width 5
                               :alu/height 4}
              :alu/pattern #{[1 5] [5 2]}}
        right {:alu/dimensions {:alu/origin [7 1]
                                :alu/width 4
                                :alu/height 3}
               :alu/pattern #{[7 1] [10 3]}}
        output (layout/merge-expressions left right)]
    (testing "Merging expressions"
      (is (= [1 1] (get-in output [:alu/dimensions :alu/origin])))
      (is (= 10 (get-in output [:alu/dimensions :alu/width])))
      (is (= 5 (get-in output [:alu/dimensions :alu/height])))
      (is (= #{[1 5] [5 2] [7 1] [10 3]} (output :alu/pattern))))
    (testing "Order does not matter"
      (is (= (layout/merge-expressions left right) (layout/merge-expressions right left))))))

(deftest spread-on-x-axis
  (let [e1 {:alu/dimensions {:alu/origin [-1 0]
                             :alu/width 4
                             :alu/height 4}
            :alu/output {:alu/position [2 3]}
            :alu/pattern #{[-1 0] [2 3]}}
        e2 {:alu/dimensions {:alu/origin [2 0]
                             :alu/width 5
                             :alu/height 4}
            :alu/output {:alu/position [6 3]}
            :alu/pattern #{[2 0] [6 3]}}
        e3 {:alu/dimensions {:alu/origin [12 0]
                             :alu/width 2
                             :alu/height 4}
            :alu/output {:alu/position [13 3]}
            :alu/pattern #{[12 0] [13 3]}}
        e4 {:alu/dimensions {:alu/origin [11 0]
                             :alu/width 2
                             :alu/height 4}
            :alu/output {:alu/position [12 3]}
            :alu/pattern #{[11 0] [12 3]}}
        [o1 o2 o3 o4] (layout/spread-x [e1 e2 e3 e4])]
    (testing "Spreaded expressions do no overlap"
      (is (= -1 (get-in o1 [:alu/dimensions :alu/origin 0])))
      (is (= 3 (get-in o2 [:alu/dimensions :alu/origin 0])))
      (is (= 8 (get-in o3 [:alu/dimensions :alu/origin 0])))
      (is (= 10 (get-in o4 [:alu/dimensions :alu/origin 0]))))
    (testing "Spreaded expressions are still within bounds"
      (is (layout/within-bounds? o1))
      (is (layout/within-bounds? o2))
      (is (layout/within-bounds? o3))
      (is (layout/within-bounds? o4)))))
