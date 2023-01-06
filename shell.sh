#!/bin/sh

echo "GOING TO $(pwd $0)"

cd "$(pwd $0)"
$HOME/bin/rebar3 shell --eval "net_kernel:start([xdtberldev@localhost,shortnames])."
