(ns tests
  (:require [babashka.process :refer [process destroy-tree]]
            [babashka.curl :as curl]
            [clojure.string :as s]))

(defn- start-calculator [file port]
  (let [cmd  ["java" "-jar" file "--port" port]
        proc (process cmd {:out :inherit
                           :shutdown destroy-tree})]
    (println (str "Starting calculator: `" (s/join " " cmd) "'"))
    (Thread/sleep 3000)
    proc))

(defn- call-endpoint [host port]
  (let [addr   (str host ":" port "/health")
        health (curl/get addr {:throw false})
        status (:status health)]
    (if (= 200 status)
      [0 "Success!"]
      [1 (str "Cannot check health, error=" (:err health))])))

(defn run [file options]
  (println (str "Processing file " file " with options " options))
  (let [port  (:port options)
        host  (:host options)
        start (not (:no-startup options))]
    (when start
      (start-calculator file port))
    (call-endpoint host port)))
