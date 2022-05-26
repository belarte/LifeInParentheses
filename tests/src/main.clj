#!/usr/bin/env bb

(ns main
  (:require [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]
            [babashka.fs :as fs]))

(def cli-options
  [["-p" "--port PORT" "Port number"
    :default 3000
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   [nil "--host HOST" "Host"
    :default "localhost"]
   ["-h" "--help" "Print this help"]])

(defn- usage [options-summary]
  (->> ["Integration tests for LifeInParentheses."
        ""
        "Usage: bb run-tests [options] calculator.jar"
        ""
        "Options:"
        options-summary]
       (string/join \newline)))

(defn- error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn- validate [arguments]
  (and
    (= 1 (count arguments))
    (fs/exists? (first arguments))))

(defn- validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with an error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options)      {:exit-message (usage summary) :ok? true}
      errors               {:exit-message (error-msg errors)}
      (validate arguments) {:file (first arguments) :options options}
      :else                {:exit-message (error-msg [(str "File `" (first arguments) "` does not exist...")])})))

(defn- exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [file options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (exit 0 (str "Processing file " file " with options " options)))))
