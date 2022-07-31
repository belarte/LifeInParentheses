(ns app.main
  (:require [clojure.tools.logging :as log]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.repl :as repl]
            [server.server :as server])
  (:gen-class))

(def cli-options
  [["-p" "--port PORT" "Port number"
    :default 3000
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]])

(defn shutdown!
  [& _]
  (log/info "Received SIGINT, Shutting down")
  (server/stop)
  (shutdown-agents)
  (System/exit 0))

(defn -main
  [& args]
  (let [port (-> (parse-opts args cli-options) :options :port)]
    (repl/set-break-handler! shutdown!)
    (server/start port)))
