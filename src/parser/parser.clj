(ns parser.parser
  (:require [instaparse.core :as p]))

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
    2 [(dictionary (nth expression 0)) (read-terminal dictionary (nth expression 1))]))

(defn- read-binary [dictionary [header & expression]]
  {:pre [(= :binary header)]}
  (case (count expression)
    1 (read-unary dictionary (first expression))
    3 [(dictionary (nth expression 1))
       (read-binary dictionary (nth expression 0))
       (read-unary dictionary (nth expression 2))]))

(defn read-expr [dictionary [header & expression]]
  {:pre [(= :expr header)]}
  (read-binary dictionary (first expression)))

(defn parser> [grammar dictionary]
  (fn [expression]
    (read-expr dictionary ((p/parser grammar) expression))))
