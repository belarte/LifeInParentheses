(ns parser.parser
  (:require [instaparse.core :as p]
            [clojure.string :as str]))

(def grammar
    "expr = binary
     binary = binary #'\\&|\\|' unary | unary
     unary = '~' terminal | terminal
     terminal = #'\\d+' | '(' expr ')'")

(declare read-expr)

(defn- read-terminal [dictionary [header & expression]]
  {:pre [(= :terminal header)]}
  (case (count expression)
    1 (seq [(dictionary "value") (Integer/parseInt (first expression))])
    3 (read-expr dictionary (nth expression 1))))

(defn- read-unary [dictionary [header & expression]]
  {:pre [(= :unary header)]}
  (case (count expression)
    1 (read-terminal dictionary (first expression))
    2 (let [operand       (dictionary (nth expression 0))
            [expr values] (read-terminal dictionary (nth expression 1))]
        [(seq [operand expr]) values])))

(defn- read-binary [dictionary [header & expression]]
  {:pre [(= :binary header)]}
  (case (count expression)
    1 (read-unary dictionary (first expression))
    3 (let [operand                 (dictionary (nth expression 1))
            [left-expr  left-vals]  (read-binary dictionary (nth expression 0))
            [right-expr right-vals] (read-unary dictionary (nth expression 2))]
        [(seq [operand left-expr right-expr]) [left-vals right-vals]])))

(defn- read-expr [dictionary [header & expression]]
  {:pre [(= :expr header)]}
  (read-binary dictionary (first expression)))

(defn parser> [grammar dictionary]
  (fn [expression]
    (let [expr   (str/replace expression #"\s" "")
          parsed ((p/parser grammar) expr)]
      (if (p/failure? parsed)
        (throw (Exception. (str "Malformed expression: '" expression "'")))
        (read-expr dictionary parsed)))))
