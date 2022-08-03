(ns tests
  (:require [babashka.process :refer [process destroy-tree]]
            [babashka.curl :as curl]
            [babashka.wait :refer [wait-for-port]]
            [clojure.string :as s]))

(defn- start-calculator [file port wait]
  (let [cmd  ["java" "-jar" file "--port" port]
        proc (process cmd {:out :inherit
                           :shutdown destroy-tree})]
    (println (str "Starting calculator: `" (s/join " " cmd) "'"))
    (wait)
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
        wait  #(wait-for-port host port {:timeout 30000 :pause 500})
        call-endpoint (create-endpoint host port)]
    (when start (start-calculator file port wait))
    (call-endpoint "/health")))
