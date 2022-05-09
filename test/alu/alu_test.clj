(ns alu.alu-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.set :as set]
            [alu.layout :as layout :refer [=>]]
            [alu.alu :as alu :refer [one zero]]))

(deftest bit-is-properly-formed
  (testing "A bit is properly formed"
    (let [expected {:alu/dimensions {:alu/origin [0 0]
                                     :alu/width 5
                                     :alu/height 5}
                    :alu/output {:alu/position [3 3]
                                 :alu/direction :bottom-right}
                    :alu/steps 0
                    :alu/pattern #{[2 1] [3 2] [1 3] [2 3] [3 3]}}
          output (=> alu/bit> 1)]
      (is (= expected output)))))

(deftest write-and-read-bit
  (testing "A bit in input can be read as output"
    (is (= 0 (alu/read> alu/bit> 0)))
    (is (= 1 (alu/read> alu/bit> 1)))))

(deftest wire-traversal
  (testing "A bit can be read after traversing a wire"
    (is (= 0 (alu/read> (layout/wire> alu/bit> 3) 0)))
    (is (= 1 (alu/read> (layout/wire> alu/bit> 3) 1)))
    (is (= 1 (alu/read> (layout/wire> (layout/wire> alu/bit> 2) 3) 1))))
  (testing "A wire can extend a flipped pattern"
    (is (= 1 (alu/read> (layout/wire> (layout/flip-x> alu/bit>) 4) 1)))))

(deftest negation-is-properly-formed
  (testing "A negation is properly formed"
    (let [output (=> (alu/not> alu/bit>) 1)]
      (is (= [0 -1]       (-> output :alu/dimensions :alu/origin)))
      (is (= 11           (-> output :alu/dimensions :alu/width)))
      (is (= 11           (-> output :alu/dimensions :alu/height)))
      (is (= [1 8]        (-> output :alu/output :alu/position)))
      (is (= :bottom-left (-> output :alu/output :alu/direction)))
      (is (= 24           (output :alu/steps)))
      (is (set/subset? #{[3 3] [7 2]} (output :alu/pattern)))))
  (testing "A double negation is properly formed"
    (let [output (=> (alu/not> (alu/not> alu/bit>)) 1)]
      (is (= [0 -2]       (-> output :alu/dimensions :alu/origin)))
      (is (= 23           (-> output :alu/dimensions :alu/width)))
      (is (= 17           (-> output :alu/dimensions :alu/height)))
      (is (= [7 13]       (-> output :alu/output :alu/position)))
      (is (= :bottom-left (-> output :alu/output :alu/direction)))
      (is (= 48           (output :alu/steps)))
      (is (set/subset? #{[3 2] [7 3] [19 1]} (output :alu/pattern))))))

(deftest negation
  (testing "Can negate a single bit"
    (let [fun (alu/not> alu/bit>)]
      (is (= 1 (alu/read> fun 0)))
      (is (= 0 (alu/read> fun 1)))))
  (testing "Can negate a flipped bit"
    (let [fun (alu/not> (layout/flip-x> alu/bit>))]
      (is (= 1 (alu/read> fun 0)))
      (is (= 0 (alu/read> fun 1)))))
  (testing "Can negate a wired bit"
    (let [fun (alu/not> (layout/wire> alu/bit> 5))]
      (is (= 1 (alu/read> fun 0)))
      (is (= 0 (alu/read> fun 1)))))
  (testing "Some combination"
    (let [fun (alu/not> (layout/flip-x>  (layout/wire> alu/bit> 5)))]
      (is (= 1 (alu/read> fun 0)))
      (is (= 0 (alu/read> fun 1)))))
  (testing "Double negation returns original value"
    (let [fun (alu/not> (alu/not> alu/bit>))]
      (is (= 0 (alu/read> fun 0)))
      (is (= 1 (alu/read> fun 1))))))

(deftest and-is-properly-formed
  (testing "And is properly formed"
    (let [output (=> (alu/and> alu/bit> alu/bit>) [1 1])]
      (is (= [0 -1]        (-> output :alu/dimensions :alu/origin)))
      (is (= 17            (-> output :alu/dimensions :alu/width)))
      (is (= 17            (-> output :alu/dimensions :alu/height)))
      (is (= [14 14]       (-> output :alu/output :alu/position)))
      (is (= :bottom-right (-> output :alu/output :alu/direction)))
      (is (= 44            (output :alu/steps)))
      (is (set/subset? #{[3 3] [9 3] [13 2] [5 11]} (output :alu/pattern)))))
  (testing "Nested ands are properly formed"
    (let [output (=> (alu/and> alu/bit> (alu/and> alu/bit> alu/bit>)) [1 [1 1]])]
      (is (= [0 -1]        (-> output :alu/dimensions :alu/origin)))
      (is (= 53            (-> output :alu/dimensions :alu/width)))
      (is (= 35            (-> output :alu/dimensions :alu/height)))
      (is (= [32 32]       (-> output :alu/output :alu/position)))
      (is (= :bottom-right (-> output :alu/output :alu/direction)))
      (is (= 116           (output :alu/steps)))
      (is (set/subset?
            #{[3 3] [21 3] [27 3] [31 2] [49 2] [23 11] [23 29]}
            (output :alu/pattern))))))

(defn and-bit-test-helper [f-left f-right]
  (are [result args] (= result (alu/read> (alu/and> f-left f-right) args))
       0 [0 0]
       0 [1 0]
       0 [0 1]
       1 [1 1]))

(deftest and-bit
  (testing "And with single bits"
    (and-bit-test-helper alu/bit> alu/bit>))
  (testing "And with wired bits"
    (and-bit-test-helper (layout/wire> alu/bit> 4) alu/bit>)
    (and-bit-test-helper alu/bit> (layout/wire> alu/bit>  4)))
  (testing "And with flipped bits"
    (and-bit-test-helper (layout/flip-x> alu/bit>) (layout/flip-x> alu/bit>))
    (and-bit-test-helper alu/bit> (layout/flip-x> alu/bit>))
    (and-bit-test-helper (layout/flip-x> alu/bit>) alu/bit>))
  (testing "Nested ands"
    (are [result args] (= result (alu/read> (alu/and> (alu/and> alu/bit> alu/bit>) (alu/and> alu/bit> alu/bit>)) args))
         0 [[0 0] [0 0]], 0 [[0 0] [0 1]], 0 [[0 0] [1 0]], 0 [[0 0] [1 1]],
         0 [[0 1] [0 0]], 0 [[0 1] [0 1]], 0 [[0 1] [1 0]], 0 [[0 1] [1 1]],
         0 [[1 0] [0 0]], 0 [[1 0] [0 1]], 0 [[1 0] [1 0]], 0 [[1 0] [1 1]],
         0 [[1 1] [0 0]], 0 [[1 1] [0 1]], 0 [[1 1] [1 0]], 1 [[1 1] [1 1]])))

(defn or-bit-test-helper [f-left f-right]
  (are [result args] (= result (alu/read> (alu/or> f-left f-right) args))
       0 [0 0]
       1 [0 1]
       1 [1 0]
       1 [1 1]))

(deftest or-bit
  (testing "Or with single bits"
    (or-bit-test-helper alu/bit> alu/bit>))
  (testing "Or with wired bits"
    (or-bit-test-helper (layout/wire> alu/bit> 4) alu/bit>)
    (or-bit-test-helper alu/bit> (layout/wire> alu/bit> 4)))
  (testing "Or with flipped bits"
    (or-bit-test-helper (layout/flip-x> alu/bit>) (layout/flip-x> alu/bit>))
    (or-bit-test-helper alu/bit> (layout/flip-x> alu/bit>))
    (or-bit-test-helper (layout/flip-x> alu/bit>) alu/bit>))
  (testing "Nested ors"
    (are [result args] (= result (alu/read> (alu/or> (alu/or> alu/bit> alu/bit>) (alu/or> alu/bit> alu/bit>)) args))
         0 [[0 0] [0 0]], 1 [[0 0] [0 1]], 1 [[0 0] [1 0]], 1 [[0 0] [1 1]],
         1 [[0 1] [0 0]], 1 [[0 1] [0 1]], 1 [[0 1] [1 0]], 1 [[0 1] [1 1]],
         1 [[1 0] [0 0]], 1 [[1 0] [0 1]], 1 [[1 0] [1 0]], 1 [[1 0] [1 1]],
         1 [[1 1] [0 0]], 1 [[1 1] [0 1]], 1 [[1 1] [1 0]], 1 [[1 1] [1 1]])))

(deftest write-and-read-byte
  (testing "A byte can be write then read again"
    (is (= 0   (alu/read-byte (alu/write-byte 0))))
    (is (= 1   (alu/read-byte (alu/write-byte 1))))
    (is (= 42  (alu/read-byte (alu/write-byte 42))))
    (is (= 86  (alu/read-byte (alu/write-byte 86))))
    (is (= 255 (alu/read-byte (alu/write-byte 255)))))
  (testing "Inputs are validated"
    (is (thrown? AssertionError (alu/write-byte -1)))
    (is (thrown? AssertionError (alu/write-byte 256)))
    (is (thrown? AssertionError (alu/read-byte [one one one one one one one])))
    (is (thrown? AssertionError (alu/read-byte [one one one one one one one one one])))))
