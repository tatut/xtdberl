#!/bin/bash -e
cd "$( dirname "${BASH_SOURCE[0]}" )"
clojure -M -m uberdeps.uberjar --deps-file ../deps.edn --target ../target/xtdb.jar --main-class xtdberl.main --aliases rocksdb:lucene:dev:log-jul
