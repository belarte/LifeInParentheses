(ns parser.parser-test
  (:require [clojure.test :refer [deftest testing is]]
            [parser.parser :as p]))

(def dictionary {"value" :byte, "~" :not, "|" :or, "&" :and})

(def parser (p/parser> p/grammar dictionary))

(deftest can-parse-logical-expression
  (testing "Can parse terminal logical expression"
    (is (= [:byte 123] (parser "123")))
    (is (= [:byte 123] (parser "(123)"))))
  (testing "Can parse unary logical expression"
    (is (= [[:not :byte] 123] (parser "~123")))
    (is (= [[:not :byte] 123] (parser "(~123)")))
    (is (= [[:not :byte] 123] (parser "~(123)"))))
  (testing "Can parse binary logical expression"
    (is (= [[:or  :byte :byte] [123 456]]  (parser "123|456")))
    (is (= [[:and :byte :byte] [123 456]] (parser "123&456"))))
  (testing "Can parse complex logical expression"
    (is (= [[:and [:or :byte :byte] :byte] [[123 456] 789]]
           (parser "123|456&789")))
    (is (= [[:and [:or :byte :byte] [:not :byte]] [[12 34] 127]]
           (parser "(12|34)&~127")))))

(deftest throws-with-malformed-expression
  (testing "Throws with malformed expression"
    (is (thrown? Exception (parser "(123")))))

(deftest parser-generates-an-evaluable-sequence
  (testing "Parser generates evaluable sequence"
    (let [bitwise-parser (p/parser> p/grammar {"value" 4, "&" +})
          [expr] (bitwise-parser "195&170")]
      (is (= 8 (eval expr))))))
