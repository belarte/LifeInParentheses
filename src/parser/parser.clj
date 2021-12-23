(ns parser.parser
  (:require [clojure.string :as str]))

(defn symbols-replacer [symbols dictionary]
  (fn [s] (str/replace s symbols dictionary)))

(def standard-replacer
  (symbols-replacer
    #"and|or"
    {"and" "bit-and", "or" "bit-or"}))

(defn evaluate [s]
  (eval (read-string s)))

(comment
  (let [s "(and 3 (or 4 2))"]
    (-> s
        standard-replacer
        evaluate)))
