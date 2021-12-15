(ns alu.alu-test
  (:require [clojure.test :refer [deftest testing is are]]
            [alu.layout :as layout]
            [alu.alu :as alu]))

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
    (and-bit-test-helper #(layout/flip-x (alu/bit %)) #(layout/flip-x (alu/bit %))))
  (testing "Nested ands"
    (are [result a b c d] (= result (alu/read-bit (alu/and-bit (alu/and-bit (alu/bit a) (alu/bit b)) (alu/and-bit (alu/bit c) (alu/bit d)))))
         0 0 0 0 0, 0 0 0 0 1, 0 0 0 1 0, 0 0 0 1 1,
         0 0 1 0 0, 0 0 1 0 1, 0 0 1 1 0, 0 0 1 1 1,
         0 1 0 0 0, 0 1 0 0 1, 0 1 0 1 0, 0 1 0 1 1,
         0 1 1 0 0, 0 1 1 0 1, 0 1 1 1 0, 1 1 1 1 1))
  (testing "Throws when expressions are not facing same direction"
    (is (thrown? AssertionError (alu/and-bit (alu/bit 0) (layout/flip-x (alu/bit 0)))))
    (is (thrown? AssertionError (alu/and-bit (layout/flip-x (alu/bit 0)) (alu/bit 0))))))

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
    (or-bit-test-helper #(layout/flip-x (alu/bit %)) #(layout/flip-x (alu/bit %))))
  (testing "Nested ors"
    (are [result a b c d] (= result (alu/read-bit (alu/or-bit (alu/or-bit (alu/bit a) (alu/bit b)) (alu/or-bit (alu/bit c) (alu/bit d)))))
         0 0 0 0 0, 1 0 0 0 1, 1 0 0 1 0, 1 0 0 1 1,
         1 0 1 0 0, 1 0 1 0 1, 1 0 1 1 0, 1 0 1 1 1,
         1 1 0 0 0, 1 1 0 0 1, 1 1 0 1 0, 1 1 0 1 1,
         1 1 1 0 0, 1 1 1 0 1, 1 1 1 1 0, 1 1 1 1 1))
  (testing "Throws when expressions are not facing same direction"
    (is (thrown? AssertionError (alu/and-bit (alu/bit 0) (layout/flip-x (alu/bit 0)))))
    (is (thrown? AssertionError (alu/and-bit (layout/flip-x (alu/bit 0)) (alu/bit 0))))))

