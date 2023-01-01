(ns xtdberl.logging
  "Configure JUL logging based on config map."
  (:import (java.util.logging LogManager))
  (:require [clojure.java.io :as io]))

(defn configure [log-config]
  (with-open [in (if log-config
                   (io/input-stream log-config)
                   (io/input-stream (io/resource "log.properties")))]

    (doto (LogManager/getLogManager)
      (.readConfiguration in))))
