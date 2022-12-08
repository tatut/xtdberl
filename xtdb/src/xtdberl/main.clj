(ns xtdberl.main
  (:require [xtdberl.core :as xtdberl])
  (:gen-class))

(defn -main [& args]
  (let [f (first args)]
    (if-not f
      (println "Usage: give configuration file name as parameter")
      (xtdberl/start (read-string (slurp f))))))
