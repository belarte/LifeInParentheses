(ns alu.alu-test
  (:require [clojure.test :refer [deftest testing is]]
            [alu.alu :as alu]))

(deftest write-and-read-bit
  (testing "A bit in input can be read as output"
    (is (= 0 (alu/output (alu/bit 0))))
    (is (= 1 (alu/output (alu/bit 1)))))
  (testing "A bit can be read after traversing a wire"
    (is (= 0 (alu/output (alu/wire (alu/bit 0) 3))))
    (is (= 1 (alu/output (alu/wire (alu/bit 1) 3))))))
