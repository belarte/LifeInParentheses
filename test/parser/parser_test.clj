(ns parser.parser-test
  (:require [clojure.test :refer [deftest testing is]]
            [parser.parser :as p]))

(deftest can-replace-symbols
  (testing "Replacing symbols"
    (let [in  "(stuff 3 (thing 4 2))"
          out "(something 3 (else 4 2))"
          replacer (p/symbols-replacer #"stuff|thing" {"stuff" "something"
                                                       "thing" "else"})]
      (is (= out (replacer in))))))

(deftest evaluating-expression
  (testing "Evaluating expression"
    (is (= 17 (p/evaluate "(bit-and 145 (bit-or 51 15))")))))
