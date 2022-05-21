(ns parser.parser
  (:require [instaparse.core :as p]))

(def parser
  (p/parser
    "expr = binary
     binary = binary #'\\&|\\|' unary | unary
     unary = '~' terminal | terminal
     terminal = #'\\d+' | '(' expr ')'"))

(def dic {"~" :not, "|" :or, "&" :and})

(declare read-expr)

(defn- read-terminal [[header & expression]]
  {:pre [(= :terminal header)]}
  (case (count expression)
    1 [:byte (Integer/parseInt (first expression))]
    3 (read-expr (nth expression 1))))

(defn- read-unary [[header & expression]]
  {:pre [(= :unary header)]}
  (case (count expression)
    1 (read-terminal (first expression))
    2 [(dic (nth expression 0)) (read-terminal (nth expression 1))]))

(defn- read-binary [[header & expression]]
  {:pre [(= :binary header)]}
  (case (count expression)
    1 (read-unary (first expression))
    3 [(dic (nth expression 1)) (read-binary (nth expression 0)) (read-unary (nth expression 2))]))

(defn read-expr [[header & expression]]
  {:pre [(= :expr header)]}
  (read-binary (first expression)))

(comment
  (parser "234"))
