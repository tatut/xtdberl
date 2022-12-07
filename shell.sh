#!/bin/sh

cd "$(pwd $0)"
$HOME/bin/rebar3 shell --eval "net_kernel:start([dev@localhost,shortnames])."
