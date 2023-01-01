#!/bin/sh

mkdir classes
clojure -M -e "(doseq [n '[xtdberl.main xtdberl.term xtdberl.core]] (compile n))"
./uberdeps/package.sh
