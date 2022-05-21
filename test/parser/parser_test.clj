(ns parser.parser-test
  (:require [clojure.test :refer [deftest testing is]]
            [parser.parser :as p]))

(deftest can-parse-logical-expression
  (testing "Can parse terminal logical expression"
    (is (= [:byte 123] (p/read-expr (p/parser "123"))))
    (is (= [:byte 123] (p/read-expr (p/parser "(123)")))))
  (testing "Can parse unary logical expression"
    (is (= [:not [:byte 123]] (p/read-expr (p/parser "~123"))))
    (is (= [:not [:byte 123]] (p/read-expr (p/parser "(~123)"))))
    (is (= [:not [:byte 123]] (p/read-expr (p/parser "~(123)")))))
  (testing "Can parse binary logical expression"
    (is (= [:or [:byte 123] [:byte 456]]  (p/read-expr (p/parser "123|456"))))
    (is (= [:and [:byte 123] [:byte 456]] (p/read-expr (p/parser "123&456")))))
  (testing "Can parse complex logical expression"
    (is (= [:and [:or [:byte 123] [:byte 456]] [:byte 789]]
           (p/read-expr (p/parser "123|456&789"))))
    (is (= [:and [:or [:byte 12] [:byte 34]] [:not [:byte 127]]]
           (p/read-expr (p/parser  "(12|34)&~127"))))))
