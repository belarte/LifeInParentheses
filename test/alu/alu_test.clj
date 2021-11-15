(ns alu.alu-test
  (:require [clojure.test :refer [deftest testing is]]
            [alu.layout :as layout]
            [alu.alu :as alu]))

(deftest write-and-read-bit
  (testing "A bit in input can be read as output"
    (is (= 0 (alu/output (alu/bit 0))))
    (is (= 1 (alu/output (alu/bit 1))))))

(deftest wire-traversal
  (testing "A bit can be read after traversing a wire"
    (is (= 0 (alu/output (layout/wire (alu/bit 0) 3))))
    (is (= 1 (alu/output (layout/wire (alu/bit 1) 3))))
    (is (= 1 (alu/output (layout/wire (layout/wire (alu/bit 1) 2) 3)))))
  (testing "A wire can extend a flipped pattern"
    (is (= 1 (alu/output (layout/wire (layout/flip-x (alu/bit 1)) 4))))))
