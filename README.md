xtdberl
=====

Use XTDB from Erlang/OTP. Contains Erlang libraries that send queries and documents to XTDB node.

The XTDB node uses jinterface to appear like an Erlang process, you send it commands and it response.

## Build the library

    $ rebar3 compile

## Run the XTDB node

The `xtdb` folder contains the Clojure project for running the database node.
The database node acts as an Erlang named process that accepts queries and commands.
