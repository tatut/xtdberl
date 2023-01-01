#!/bin/sh

CONFIG=${1:-dev}

clj -M:dev:log-jul:$CONFIG -m xtdberl.main $CONFIG.config.edn
