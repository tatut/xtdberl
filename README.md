xtdberl
=====

*** ALPHA status, don't use this yet! ***


Use [XTDB](https://xtdb.com) from Erlang/OTP. Contains Erlang libraries that send queries and documents to XTDB node.

The XTDB node uses jinterface to appear like an Erlang process, you send it commands and it responds.

Features:
- Map Erlang records to XTDB documents
- Query by giving candidate instances
- Query by text search and comparison operators

## Documentation

Automatically generated edoc is available [here](https://tatut.github.io/xtdberl/).

## Quickstart

Open two shells, one for Erlang shell and one for the XTDB node.
Prerequisites:
- [Erlang/OTP](https://www.erlang.org/downloads) (tested with 25)
- [Clojure](https://clojure.org/guides/install_clojure)

Note that you currently need to build the jinterface OtpErlang.jar and copy it to the `xtdb` folder
(see xtdb/deps.edn). A recent version of jinterface is not available on maven central or clojars.


Start Erlang in the project folder:
```shell
$ rebar3 compile
$ ./shell.sh
```

Start the XTDB node in the xtdb folder:
```shell
$ ./run.sh rocksdb
```

Then you can use the library in the Erlang shell:
```erlang

%% Register mappings and add some example data
1> xt_demo:init().
ok

%% Define records in shell for easy access
2> rr(xt_demo).
[address,person]

%% Use qlike to query
3> xt:ql(#person{first_name="Annika"}).
[#person{person_id = "456456-666",first_name = "Annika",
         last_name = "Andersson",email = "aa@existerar-inte.se",
         date_of_birth = {1986,5,27},
         shipping_address = undefined,
         billing_address = #address{street = "Svensk gatan 6",
                                    city = "Stockholm",zip = "666123",country = ':SE'}}]

%% Add new person
4> xt:put(#person{person_id="demo1", first_name="Donna", last_name="Demonstration"}).
{ok,...}

%% Query also works by id
5> xt:ql(#person{person_id="demo1"})
[#person{person_id = "demo1",first_name = "Donna",
         last_name = "Demonstration",email = undefined,
         date_of_birth = undefined,shipping_address = undefined,
         billing_address = undefined}]

```

You can browse what is stored in the XTDB database using the included inspector.
Just open a web browser to [http://localhost:3000/tx](http://localhost:3000/tx).

## Build the library

    $ rebar3 compile

## Run the XTDB node

The `xtdb` folder contains the Clojure project for running the database node.
The database node acts as an Erlang named process that accepts queries and commands.

## TODO

Here are some planned features:

* Projection (pull only some fields)
* Query ids and pull by ids (for doing fine grained pagination things)
* Batching results (send results back by messages of wanted size)
* Mapping for linked documents (query and pull them)
