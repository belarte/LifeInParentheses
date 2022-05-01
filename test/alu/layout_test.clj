(ns alu.layout-test
  (:require [clojure.test :refer [deftest testing is are]]
            [alu.layout :as layout]))

(def left-facing {:alu/dimensions {:alu/origin [1 1]
                                   :alu/width 6
                                   :alu/height 3}
                  :alu/output {:alu/position [2 3]
                               :alu/direction :bottom-left}
                  :alu/steps 16
                  :alu/pattern #{[4 1] [3 2]}})

(def right-facing {:alu/dimensions {:alu/origin [1 1]
                                    :alu/width 6
                                    :alu/height 3}
                   :alu/output {:alu/position [5 3]
                                :alu/direction :bottom-right}
                   :alu/steps 16
                   :alu/pattern #{[3 1] [4 2]}})

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

(defn within-bounds-test-helper [pattern output]
  {:alu/dimensions {:alu/origin [2 2]
                    :alu/width 5
                    :alu/height 4}
   :alu/output {:alu/position output}
   :alu/pattern pattern})

(deftest within-bounds
  (testing "Expression is within the bounds"
    (is (true? (layout/within-bounds? (within-bounds-test-helper #{[2 2] [6 2] [5 2] [6 5]} [6 5])))))
  (testing "Patterns are not within the bounds"
    (are [pattern] (false? (layout/within-bounds? (within-bounds-test-helper pattern [6 5])))
         #{[1 2]}
         #{[2 1]}
         #{[7 5]}
         #{[6 6]}))
  (testing "Outputs are not within the bounds"
    (are [output] (false? (layout/within-bounds? (within-bounds-test-helper #{} output)))
         [1 2]
         [2 1]
         [7 5]
         [6 6])))

(deftest flip-x
  (testing "Flipping twice returns copy of input"
    (is (= left-facing (layout/flip-x (layout/flip-x left-facing))))
    (is (= right-facing (layout/flip-x (layout/flip-x right-facing)))))
  (testing "Can flip an expression on the X axis"
    (is (= left-facing (layout/flip-x right-facing)))
    (is (= right-facing (layout/flip-x left-facing)))))

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

(deftest align-with-origin
  (let [e {:alu/dimensions {:alu/origin [1 2]
                            :alu/width 5
                            :alu/height 5}
           :alu/output {:alu/position [5 6]}
           :alu/pattern #{[1 2] [2 3] [3 4] [4 5]}}
        aligned (layout/align-with-origin e)]
    (testing "Check new origin is [0 0]"
      (is (= [0 0] (get-in aligned [:alu/dimensions :alu/origin]))))
    (testing "New output is correct"
      (is (= [4 4] (get-in aligned [:alu/output :alu/position]))))
    (testing "Aligned expression is still within bounds"
      (is (layout/within-bounds? aligned)))))

(deftest change-direction
  (testing "Change direction"
    (is (= right-facing (layout/change-direction :bottom-right right-facing)))
    (is (= right-facing (layout/change-direction :bottom-right left-facing)))
    (is (= left-facing (layout/change-direction :bottom-left right-facing)))
    (is (= left-facing (layout/change-direction :bottom-left left-facing)))))

(deftest make-intersect
  (let [[left-output right-output] (layout/make-intersect left right)]
    (testing "Result expressions do not overlap"
      (let [[xl] (-> left-output :alu/dimensions :alu/origin)
            [xr] (-> right-output :alu/dimensions :alu/origin)
            w    (-> left-output :alu/dimensions :alu/width)]
        (is (>= xr (+ xl w)))))
    (testing "X origin coordinates are separated by an odd number of cells"
      (let [[xl] (-> left-output :alu/output :alu/position)
            [xr] (-> right-output :alu/output :alu/position)]
        (is (odd? (- xr xl 1)))))
    (testing "Both expressiona have same number of steps"
      (let [left-steps  (left-output :alu/steps)
            right-steps (right-output :alu/steps)]
        (is (= left-steps right-steps))))
    (testing "Outputs align"
      (let [[_ yl] (-> left-output :alu/output :alu/position)
            [_ yr] (-> right-output :alu/output :alu/position)]
        (is (= yr (- yl 1)))))
    (testing "Inputs are flipped so left is facing right and right is facing left"
      (is (= (layout/make-intersect right right-facing) (layout/make-intersect right left-facing)))
      (is (= (layout/make-intersect right-facing left) (layout/make-intersect left-facing left))))
    (testing "Organise epressions so first is going bottom-right and second bottom-left"
      (is (= :bottom-right (-> left-output :alu/output :alu/direction)))
      (is (= :bottom-left (-> right-output :alu/output :alu/direction)))))
  (let [left {:alu/dimensions {:alu/origin [18 -1]
                               :alu/width 17
                               :alu/height 17}
              :alu/output {:alu/position [32 14]
                           :alu/direction :bottom-right}
              :alu/steps 44}
        right {:alu/dimensions {:alu/origin [0 0]
                                :alu/width 5
                                :alu/height 5}
               :alu/output {:alu/position [3 3]
                            :alu/direction :bottom-right}
               :alu/steps 0}
        [l r] (layout/make-intersect left right)]
    (testing "Left output is correct"
      (is (= [18 -1]       (-> l :alu/dimensions :alu/origin)))
      (is (= 17            (-> l :alu/dimensions :alu/width)))
      (is (= 17            (-> l :alu/dimensions :alu/height)))
      (is (= [32 14]       (-> l :alu/output :alu/position)))
      (is (= :bottom-right (-> l :alu/output :alu/direction)))
      (is (= 44            (l :alu/steps))))
    (testing "Right output is correct"
      (is (= [37 -1]      (-> r :alu/dimensions :alu/origin)))
      (is (= 16           (-> r :alu/dimensions :alu/width)))
      (is (= 16           (-> r :alu/dimensions :alu/height)))
      (is (= [38 13]      (-> r :alu/output :alu/position)))
      (is (= :bottom-left (-> r :alu/output :alu/direction)))
      (is (= 44           (r :alu/steps))))))

(deftest make-parallel
  (let [[left-output right-output] (layout/make-parallel left right)]
    (testing "Result expressions do not overlap"
      (let [[xl] (-> left-output :alu/dimensions :alu/origin)
            [xr] (-> right-output :alu/dimensions :alu/origin)
            w    (-> left-output :alu/dimensions :alu/width)]
        (is (>= xr (+ xl w)))))
    (testing "X origin coordinates are separated by an odd number of cells"
      (let [[xl] (-> left-output :alu/output :alu/position)
            [xr] (-> right-output :alu/output :alu/position)]
        (is (odd? (- xr xl 1)))))
    (testing "Both expressiona have same number of steps"
      (let [left-steps  (left-output :alu/steps)
            right-steps (right-output :alu/steps)]
        (is (= left-steps right-steps))))
    (testing "Outputs align"
      (let [[_ yl] (-> left-output :alu/output :alu/position)
            [_ yr] (-> right-output :alu/output :alu/position)]
        (is (= yr yl))))
    (testing "Inputs are flipped so both are facing right"
      (is (= (layout/make-parallel right right-facing) (layout/make-parallel right left-facing)))
      (is (= (layout/make-parallel right-facing left) (layout/make-parallel left-facing left))))
    (testing "Organise epressions so both face bottom right"
      (is (= :bottom-right (-> left-output :alu/output :alu/direction)))
      (is (= :bottom-right (-> right-output :alu/output :alu/direction)))))
  (let [left {:alu/dimensions {:alu/origin [0 0]
                               :alu/width 5
                               :alu/height 5}
              :alu/output {:alu/position [3 3]
                           :alu/direction :bottom-right}
              :alu/steps 0}
        right {:alu/dimensions {:alu/origin [0 -1]
                                :alu/width 17
                                :alu/height 17}
               :alu/output {:alu/position [14 14]
                            :alu/direction :bottom-right}
               :alu/steps 44}
        [l r] (layout/make-parallel left right)]
    (testing "Left output is correct"
      (is (= [0 0]         (-> l :alu/dimensions :alu/origin)))
      (is (= 16            (-> l :alu/dimensions :alu/width)))
      (is (= 16            (-> l :alu/dimensions :alu/height)))
      (is (= [14 14]       (-> l :alu/output :alu/position)))
      (is (= :bottom-right (-> l :alu/output :alu/direction)))
      (is (= 44            (l :alu/steps))))
    (testing "Right output is correct"
      (is (= [18 -1]       (-> r :alu/dimensions :alu/origin)))
      (is (= 17            (-> r :alu/dimensions :alu/width)))
      (is (= 17            (-> r :alu/dimensions :alu/height)))
      (is (= [32 14]       (-> r :alu/output :alu/position)))
      (is (= :bottom-right (-> r :alu/output :alu/direction)))
      (is (= 44            (r :alu/steps))))))

(deftest merge-expressions
  (let [e1 {:alu/dimensions {:alu/origin [1 2]
                             :alu/width 5
                             :alu/height 4}
            :alu/pattern #{[1 5] [5 2]}}
        e2 {:alu/dimensions {:alu/origin [7 1]
                             :alu/width 4
                             :alu/height 3}
            :alu/pattern #{[7 1] [10 3]}}
        e3 {:alu/dimensions {:alu/origin [3 3]
                             :alu/width 5
                             :alu/height 4}
            :alu/pattern #{[3 3] [7 6]}}
        o1 (layout/merge-expressions e1 e2)
        o2 (layout/merge-expressions e1 e2 e3)]
    (testing "Merging two expressions"
      (is (= [1 1] (-> o1 :alu/dimensions :alu/origin)))
      (is (= 10    (-> o1 :alu/dimensions :alu/width)))
      (is (= 5     (-> o1 :alu/dimensions :alu/height)))
      (is (= #{[1 5] [5 2] [7 1] [10 3]} (o1 :alu/pattern))))
    (testing "Merging more than two expressions"
      (is (= [1 1] (-> o2 :alu/dimensions :alu/origin)))
      (is (= 10    (-> o2 :alu/dimensions :alu/width)))
      (is (= 6     (-> o2 :alu/dimensions :alu/height)))
      (is (= #{[1 5] [5 2] [7 1] [10 3] [3 3] [7 6]} (o2 :alu/pattern))))
    (testing "Order does not matter"
      (is (= (layout/merge-expressions e1 e2) (layout/merge-expressions e2 e1))))))

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
