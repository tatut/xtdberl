#!/bin/sh

CONFIG=${1:-dev}

clj -M:dev:$CONFIG -m xtdberl.main $CONFIG.config.edn
