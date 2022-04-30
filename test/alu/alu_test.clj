(ns alu.alu-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure.set :as set]
            [alu.layout :as layout]
            [alu.alu :as alu]))

(deftest bit-is-properly-formed
  (testing "A bit is properly formed"
    (let [expected {:alu/dimensions {:alu/origin [0 0]
                                     :alu/width 5
                                     :alu/height 5}
                    :alu/output {:alu/position [3 3]
                                 :alu/direction :bottom-right}
                    :alu/steps 0
                    :alu/pattern #{[2 1] [3 2] [1 3] [2 3] [3 3]}}]
      (is (= expected (alu/bit 1))))))

(deftest write-and-read-bit
  (testing "A bit in input can be read as output"
    (is (= 0 (alu/read-bit (alu/bit 0))))
    (is (= 1 (alu/read-bit (alu/bit 1))))))

(deftest wire-traversal
  (testing "A bit can be read after traversing a wire"
    (is (= 0 (alu/read-bit (layout/wire (alu/bit 0) 3))))
    (is (= 1 (alu/read-bit (layout/wire (alu/bit 1) 3))))
    (is (= 1 (alu/read-bit (layout/wire (layout/wire (alu/bit 1) 2) 3)))))
  (testing "A wire can extend a flipped pattern"
    (is (= 1 (alu/read-bit (layout/wire (layout/flip-x (alu/bit 1)) 4))))))

(deftest negation-is-properly-formed
  (testing "A negation is properly formed"
    (let [output (alu/not-bit (alu/bit 1))]
      (is (= [0 -1]       (-> output :alu/dimensions :alu/origin)))
      (is (= 11           (-> output :alu/dimensions :alu/width)))
      (is (= 11           (-> output :alu/dimensions :alu/height)))
      (is (= [1 8]        (-> output :alu/output :alu/position)))
      (is (= :bottom-left (-> output :alu/output :alu/direction)))
      (is (= 24           (output :alu/steps)))
      (is (set/subset? #{[3 3] [7 2]} (output :alu/pattern)))))
  (testing "A double negation is properly formed"
    (let [output (alu/not-bit (alu/not-bit (alu/bit 1)))]
      (is (= [0 -2]       (-> output :alu/dimensions :alu/origin)))
      (is (= 23           (-> output :alu/dimensions :alu/width)))
      (is (= 17           (-> output :alu/dimensions :alu/height)))
      (is (= [7 13]       (-> output :alu/output :alu/position)))
      (is (= :bottom-left (-> output :alu/output :alu/direction)))
      (is (= 48           (output :alu/steps)))
      (is (set/subset? #{[3 2] [7 3] [19 1]} (output :alu/pattern))))))

(deftest negation
  (testing "Can negate a single bit"
    (is (= 1 (alu/read-bit (alu/not-bit (alu/bit 0)))))
    (is (= 0 (alu/read-bit (alu/not-bit (alu/bit 1))))))
  (testing "Can negate a flipped bit"
    (is (= 1 (alu/read-bit (alu/not-bit (layout/flip-x (alu/bit 0))))))
    (is (= 0 (alu/read-bit (alu/not-bit (layout/flip-x (alu/bit 1)))))))
  (testing "Can negate a wired bit"
    (is (= 1 (alu/read-bit (alu/not-bit (layout/wire (alu/bit 0) 5)))))
    (is (= 0 (alu/read-bit (alu/not-bit (layout/wire (alu/bit 1) 5))))))
  (testing "Some combination"
    (is (= 1 (alu/read-bit (alu/not-bit (layout/flip-x (layout/wire (alu/bit 0) 5))))))
    (is (= 0 (alu/read-bit (alu/not-bit (layout/flip-x (layout/wire (alu/bit 1) 5)))))))
  (testing "Double negation returns original value"
    (is (= 0 (alu/read-bit (alu/not-bit (alu/not-bit (alu/bit 0))))))
    (is (= 1 (alu/read-bit (alu/not-bit (alu/not-bit (alu/bit 1))))))))

(deftest and-is-properly-formed
  (testing "And is properly formed"
    (let [output (alu/and-bit (alu/bit 1) (alu/bit 1))]
      (is (= [0 -1]        (-> output :alu/dimensions :alu/origin)))
      (is (= 17            (-> output :alu/dimensions :alu/width)))
      (is (= 17            (-> output :alu/dimensions :alu/height)))
      (is (= [14 14]       (-> output :alu/output :alu/position)))
      (is (= :bottom-right (-> output :alu/output :alu/direction)))
      (is (= 44            (output :alu/steps)))
      (is (set/subset? #{[3 3] [9 3] [13 2] [5 11]} (output :alu/pattern)))))
  (testing "Nested ands are properly formed"
    (let [output (alu/and-bit (alu/bit 1) (alu/and-bit (alu/bit 1) (alu/bit 1)))]
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
  (are [result l r] (= result (alu/read-bit (alu/and-bit (f-left l) (f-right r))))
       0 0 0
       0 1 0
       0 0 1
       1 1 1))

(deftest and-bit
  (testing "And with single bits"
    (and-bit-test-helper alu/bit alu/bit))
  (testing "And with wired bits"
    (and-bit-test-helper #(layout/wire (alu/bit %) 4) alu/bit)
    (and-bit-test-helper alu/bit #(layout/wire (alu/bit %) 4)))
  (testing "And with flipped bits"
    (and-bit-test-helper #(layout/flip-x (alu/bit %)) #(layout/flip-x (alu/bit %)))
    (and-bit-test-helper #(alu/bit %) #(layout/flip-x (alu/bit %)))
    (and-bit-test-helper #(layout/flip-x (alu/bit %)) #(alu/bit %)))
  (testing "Nested ands"
    (are [result a b c d] (= result (alu/read-bit (alu/and-bit (alu/and-bit (alu/bit a) (alu/bit b)) (alu/and-bit (alu/bit c) (alu/bit d)))))
         0 0 0 0 0, 0 0 0 0 1, 0 0 0 1 0, 0 0 0 1 1,
         0 0 1 0 0, 0 0 1 0 1, 0 0 1 1 0, 0 0 1 1 1,
         0 1 0 0 0, 0 1 0 0 1, 0 1 0 1 0, 0 1 0 1 1,
         0 1 1 0 0, 0 1 1 0 1, 0 1 1 1 0, 1 1 1 1 1)))

(defn or-bit-test-helper [f-left f-right]
  (are [result l r] (= result (alu/read-bit (alu/or-bit (f-left l) (f-right r))))
       0 0 0
       1 1 0
       1 0 1
       1 1 1))

(deftest or-bit
  (testing "Or with single bits"
    (or-bit-test-helper alu/bit alu/bit))
  (testing "Or with wired bits"
    (or-bit-test-helper #(layout/wire (alu/bit %) 4) alu/bit)
    (or-bit-test-helper alu/bit #(layout/wire (alu/bit %) 4)))
  (testing "Or with flipped bits"
    (or-bit-test-helper #(layout/flip-x (alu/bit %)) #(layout/flip-x (alu/bit %)))
    (or-bit-test-helper #(alu/bit %) #(layout/flip-x (alu/bit %)))
    (or-bit-test-helper #(layout/flip-x (alu/bit %)) #(alu/bit %)))
  (testing "Nested ors"
    (are [result a b c d] (= result (alu/read-bit (alu/or-bit (alu/or-bit (alu/bit a) (alu/bit b)) (alu/or-bit (alu/bit c) (alu/bit d)))))
         0 0 0 0 0, 1 0 0 0 1, 1 0 0 1 0, 1 0 0 1 1,
         1 0 1 0 0, 1 0 1 0 1, 1 0 1 1 0, 1 0 1 1 1,
         1 1 0 0 0, 1 1 0 0 1, 1 1 0 1 0, 1 1 0 1 1,
         1 1 1 0 0, 1 1 1 0 1, 1 1 1 1 0, 1 1 1 1 1)))

