(ns alu.alu-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.set :as set]
            [alu.layout :as layout :refer [=>]]
            [alu.alu :as alu]))

(defn read-bit>
  "Reads a single bit as the output of an expression."
  [expression args]
  (let [exp       (=> expression args)
        output    (-> exp :alu/output :alu/position)
        last-iter (last (alu/evaluate exp))]
    (if (contains? (last-iter :alive-cells) output) 1 0)))

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
    (is (= 0 (read-bit> alu/bit> 0)))
    (is (= 1 (read-bit> alu/bit> 1)))))

(deftest wire-traversal
  (testing "A bit can be read after traversing a wire"
    (is (= 0 (read-bit> (layout/wire> alu/bit> 3) 0)))
    (is (= 1 (read-bit> (layout/wire> alu/bit> 3) 1)))
    (is (= 1 (read-bit> (layout/wire> (layout/wire> alu/bit> 2) 3) 1))))
  (testing "A wire can extend a flipped pattern"
    (is (= 1 (read-bit> (layout/wire> (layout/flip-x> alu/bit>) 4) 1)))))

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
      (is (= 1 (read-bit> fun 0)))
      (is (= 0 (read-bit> fun 1)))))
  (testing "Can negate a flipped bit"
    (let [fun (alu/not> (layout/flip-x> alu/bit>))]
      (is (= 1 (read-bit> fun 0)))
      (is (= 0 (read-bit> fun 1)))))
  (testing "Can negate a wired bit"
    (let [fun (alu/not> (layout/wire> alu/bit> 5))]
      (is (= 1 (read-bit> fun 0)))
      (is (= 0 (read-bit> fun 1)))))
  (testing "Some combination"
    (let [fun (alu/not> (layout/flip-x>  (layout/wire> alu/bit> 5)))]
      (is (= 1 (read-bit> fun 0)))
      (is (= 0 (read-bit> fun 1)))))
  (testing "Double negation returns original value"
    (let [fun (alu/not> (alu/not> alu/bit>))]
      (is (= 0 (read-bit> fun 0)))
      (is (= 1 (read-bit> fun 1))))))

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
  (are [result args] (= result (read-bit> (alu/and> f-left f-right) args))
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
    (are [result args] (= result (read-bit> (alu/and> (alu/and> alu/bit> alu/bit>) (alu/and> alu/bit> alu/bit>)) args))
         0 [[0 0] [0 0]], 0 [[0 0] [0 1]], 0 [[0 0] [1 0]], 0 [[0 0] [1 1]],
         0 [[0 1] [0 0]], 0 [[0 1] [0 1]], 0 [[0 1] [1 0]], 0 [[0 1] [1 1]],
         0 [[1 0] [0 0]], 0 [[1 0] [0 1]], 0 [[1 0] [1 0]], 0 [[1 0] [1 1]],
         0 [[1 1] [0 0]], 0 [[1 1] [0 1]], 0 [[1 1] [1 0]], 1 [[1 1] [1 1]])))

(defn or-bit-test-helper [f-left f-right]
  (are [result args] (= result (read-bit> (alu/or> f-left f-right) args))
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
    (are [result args] (= result (read-bit> (alu/or> (alu/or> alu/bit> alu/bit>) (alu/or> alu/bit> alu/bit>)) args))
         0 [[0 0] [0 0]], 1 [[0 0] [0 1]], 1 [[0 0] [1 0]], 1 [[0 0] [1 1]],
         1 [[0 1] [0 0]], 1 [[0 1] [0 1]], 1 [[0 1] [1 0]], 1 [[0 1] [1 1]],
         1 [[1 0] [0 0]], 1 [[1 0] [0 1]], 1 [[1 0] [1 0]], 1 [[1 0] [1 1]],
         1 [[1 1] [0 0]], 1 [[1 1] [0 1]], 1 [[1 1] [1 0]], 1 [[1 1] [1 1]])))

(deftest write-and-read-byte
  (testing "A byte can be write then read again"
    (are [result arg] (= result (alu/read> alu/byte> arg))
      0 0, 1 1, 42 42, 86 86, 255 255))
  (testing "Inputs are validated"
    (is (thrown? AssertionError (alu/read> alu/byte> -1)))
    (is (thrown? AssertionError (alu/read> alu/byte> 256)))))

(deftest not-byte
  (testing "Byte negation"
    (let [op (alu/not> alu/byte>)]
      (are [result arg] (= result (alu/read> op arg))
        255 0, 0 255, 2r10101010 2r01010101, 2r00001111 2r11110000)))
  (testing "Double negation"
    (let [op (alu/not> (alu/not> alu/byte>))]
      (are [result arg] (= result (alu/read> op arg))
        0 0, 1 1, 42 42, 86 86, 255 255))))

(deftest and-byte
  (testing "Simple binary and"
    (let [op (alu/and> alu/bit> alu/bit>)]
      (are [result args] (= result (alu/read> op args))
        0          [0 0]
        0          [0 255]
        1          [1 255]
        42         [42 255]
        2r00000011 [2r00001111 2r00110011]
        2r00001010 [2r00001111 2r10101010]
        2r10000010 [2r10101010 2r11000011])))
  (testing "Nested and"
    (let [op (alu/and> alu/bit> (alu/and> alu/bit> alu/bit>))]
      (are [result args] (= result (alu/read> op args))
        0          [0   [1 2]]
        0          [255 [0 42]]
        1          [63  [1 255]]
        2r00000001 [2r01010101 [2r00001111 2r00110011]]
        2r00001010 [2r00111111 [2r00001111 2r10101010]]
        2r10000010 [2r11111110 [2r10101010 2r11000011]]))))

(deftest or-byte
  (testing "Simple binary or"
    (let [op (alu/or> alu/bit> alu/bit>)]
      (are [result args] (= result (alu/read> op args))
        0          [0 0]
        255        [0 255]
        1          [1 0]
        2r00111111 [2r00001111 2r00110011]
        2r10101111 [2r00001111 2r10101010]
        2r11101011 [2r10101010 2r11000011])))
  (testing "Nested or"
    (let [op (alu/or> alu/bit> (alu/or> alu/bit> alu/bit>))]
      (are [result args] (= result (alu/read> op args))
        3          [0   [1 2]]
        255        [255 [0 42]]
        255        [63  [64 128]]
        2r01111111 [2r01010101 [2r00001111 2r00110011]]
        2r10111111 [2r00111111 [2r00001111 2r10101010]]
        2r11111011 [2r11110000 [2r10101010 2r11000011]]))))
