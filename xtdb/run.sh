#!/bin/sh

CONFIG=${1:-dev}

clojure -M:dev:log-jul:$CONFIG -m xtdberl.main $CONFIG.config.edn
