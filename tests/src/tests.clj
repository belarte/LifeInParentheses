(ns tests
  (:require [babashka.process :refer [process destroy-tree]]
            [babashka.curl :as curl]
            [babashka.wait :refer [wait-for-port]]
            [clojure.string :as s]))

(defn- start-calculator [file port]
  (let [cmd  ["java" "-jar" file "--port" port]
        proc (process cmd {:out :inherit
                           :shutdown destroy-tree})]
    (println (str "Starting calculator: `" (s/join " " cmd) "'"))
    (wait-for-port "localhost" port {:timeout 30000 :pause 500})
    proc))

(defn- create-endpoint [host port]
  (fn [enpoint]
    (let [url    (str host ":" port enpoint)
          health (curl/get url {:throw false})
          status (:status health)]
      (println (str "Calling " url))
      (if (= 200 status)
        [0 "Success!"]
        [1 (str "Cannot check health, error=" (:err health))]))))

(defn run [file options]
  (println (str "Processing file " file " with options " options))
  (let [port  (:port options)
        host  (:host options)
        start (not (:no-startup options))
        call-endpoint (create-endpoint host port)]
    (when start
      (start-calculator file port))
    (call-endpoint "/health")))
