(ns xtdberl.main
  (:require [xtdberl.core :as xtdberl]
            [clojure.tools.logging :as log]

            [clojure.java.io :as io])
  (:gen-class))

(defn- suppress-err [func]
  (binding [*err* (java.io.PrintWriter. (java.io.Writer/nullWriter))]
    (try
      (func)
      (finally
        :ok))))

(defn- prep-modules []
  ;; Suppress error output and require namespaces that cause ugly
  ;; "WARNING: x already refers to y in ns..." output when starting
  (doseq [m '[medley.core
              clojure.tools.analyzer.utils
              clojure.tools.analyzer
              clojure.tools.analyzer.passes
              clojure.tools.analyzer.passes.uniquify]]
    (suppress-err #(require m))))

(def demo-config
  {:xtdb {:xtdb.lucene/lucene-store {}}
   :mbox {:node "xtdb@localhost"
          :mbox "xtdb"}
   :server {:workers 20}
   :xtdb-inspector {:port 3000}})


(defn -main [& args]
  (let [f (first args)]
    (cond
      (nil? f)
      (println "Usage: give configuration file name as parameter\n"
               "Or \"new <file.edn>\" to create a new configuration file\n"
               "use \"demo\" to run an in-memory demo/test with default configuration")

      (= "new" f)
      (let [n (or (second args) "config.edn")
            config (io/file n)]
        (println "Creating new config file: " n)
        (if (.exists config)
          (println "Already exists, abort.")
          (io/copy (slurp (io/resource "new.config.edn")) config)))

      (= "demo" f)
      (do
        (prep-modules)
        (println "Running in-memory node for demonstration and testing purposes:\nConfig:\n-----\n"
                 (pr-str demo-config)
                 "\n-----\n")
        (xtdberl/start demo-config))

      :else
      (do
        (prep-modules)
        (xtdberl/start (read-string (slurp f)))))))
