(ns tests
  (:require [babashka.process :refer [process destroy-tree]]
            [babashka.curl :as curl]))


(defn- start-calculator [file]
  (let [p (process ["java" "-jar" file] {:out :inherit
                                         :shutdown destroy-tree})]
    (Thread/sleep 3000)
    p))

(defn run [file options]
  (println (str "Processing file " file " with options " options))
  (start-calculator file)
  (let [health (curl/get "localhost:3000/health" {:throw false})
        status (:status health)]
    (if (= 200 status)
      [0 "Success!"]
      [1 (str "Cannot check health, status=" status)])))
