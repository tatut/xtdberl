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
> xt_demo:init().
ok

> xt_demo:add_data().
{ok,...}

%% Define records in shell for easy access
> rr(xt_demo).
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
> xt:put(#person{person_id="demo1", first_name="Donna", last_name="Demonstration"}).
{ok,...}

%% Query also works by id
> xt:ql(#person{person_id="demo1"})
[#person{person_id = "demo1",first_name = "Donna",
         last_name = "Demonstration",email = undefined,
         date_of_birth = undefined,shipping_address = undefined,
         billing_address = undefined}]

%% And by query operators, like textsearch (see below)
> xt:ql(#person{first_name={textsearch,"Ma*"}}).
[#person{person_id = "01234-abc",first_name = "Max",
         last_name = "Feedpressure",email = "max@example.com",
         date_of_birth = {1981,4,8},
         shipping_address = undefined,billing_address = undefined},
 #person{person_id = "234234-444",first_name = "Matti",
         last_name = "Korhonen",
         email = "matti@notreallyexisting.org",
         date_of_birth = {1969,12,20},
         shipping_address = undefined,
         billing_address = #address{street = "Isokatu 25",
                                    city = "Oulu",zip = "90100",
                                    country = ':FI'}}]

```

You can browse what is stored in the XTDB database using the included inspector.
Just open a web browser to [http://localhost:3000/tx](http://localhost:3000/tx).

## Query support

The library supports sending raw Datalog queries to XTDB and a higher level
mapping from Erlang records to XTDB documents.

Erlang records are queried with the `xt:ql` function by giving it a candidate
instance. The query will return all instances that match the given candidate.
The candidate field values can be direct values (eg. numbers, strings, dates and
so on) or tuples that describe finer grained query operators.

The supported operators are:

| Tuple | Description |
| ----- | ----------- |
| `{'<', Val}` |  attr has value less than Val |
| `{'<=', Val}` | attr has value less than or equal to Val |
| `{'>', Val}` | attr has value greater than Val |
| `{'>=', Val}` |  attr has value greater than or equal to Val |
| `{between,Low,High}` | attr value is between low (inclusive) and high (inclusive) |
| `{textsearch,Term}` | attr value matches Lucene text search pattern Term |
| `{in,[Val1,...,ValN]}` | attr value is one of the Val1,...,ValN options |

By default all attributes that have a mapping will be pulled when querying
instances. Support for restricting fetched attributes is coming soon!

## Selecting partial data

The default mode is to fetch all fields that have a mapping from the database. Sometimes you would
to reduce the amount of data that needs to be fetched to optimize the
query and network load.

This can be done by passing in the `fetch` option to `xt:ql/2`.
Fetch takes a list of field references and only fetches those.
Fields in embedded records can be specified using a tuple `{#parent.child_field, [#child.field1, ...]}`.

Example:
```erlang
%% Fetch person name and city and zip of billing address for all
%% people whose  billing address is in Finland
> xt:ql(#person{billing_address=#address{country=':FI'}},
        [{fetch, [#person.first_name, #person.last_name,
                  {#person.billing_address, [#address.city, #address.zip]}]}]).
[#person{person_id = "234234-444",first_name = "Matti",
         last_name = "Korhonen",email = undefined,
         date_of_birth = undefined,shipping_address = undefined,
         billing_address = #address{street = undefined,city = "Oulu",
                                    zip = "90100",country = undefined}},
 #person{person_id = "42069-xxx",first_name = "Foo",
         last_name = "Barsky",email = undefined,
         date_of_birth = undefined,shipping_address = undefined,
         billing_address = #address{street = undefined,city = "Oulu",
                                    zip = "90420",country = undefined}}]
```

Not that even with fetch, the query will still return records but
all the unfetched fields will be undefined. The document id will
always be fetched even if it isn't specified.

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
