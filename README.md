xtdberl
=====

ARCHIVE NOTE: No plans to continue development as this is for XTDB v1.

*** ALPHA status, don't use this yet! ***


Use [XTDB](https://xtdb.com) from Erlang/OTP. Contains Erlang libraries that send queries and documents to XTDB node.

The XTDB node uses jinterface to appear like an Erlang process, you send it commands and it responds.

Features:
- Map Erlang records or Elixir structs to XTDB documents
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

## Async query

The default mode of query is to wait for the results in the process that called
query function. This can be overridden by using `{defer, Pid}` in the options.
The query will then return immediately `{deferred, QueryId}` where QueryId is
a unique reference.

Later the query results are sent to the given pid as a tuple of:
* `{ok, QueryId, [Results...]}` when success
* `{error, QueryId, ErrorInfo}` when query fails
* `{timeout, QueryId}` if query didn't respond within the timeout

Asynchronous queries can also be used to listen for changes.
Adding the `{listen, yes}` option to a deferred query will add
a transaction listener on the database. Whenever new transactions
are indexed, the query will be re-run and results sent to the same
process. When the process exits for any reason, the listener is removed.

Note: the listen parameter will in the future support patterns that
conditionally re-run queries if the pattern matches a transaction operation.

## Time travel

As XTDB retains full history of all data, it is possible to query
by giving a transaction id or time as well as a valid time.

See [XTDB documentation on bitemporality](https://docs.xtdb.com/concepts/bitemporality/).

Use the following options to query:

* `tx_time`  a tuple of `{timestamp, EpochMillis}`
* `tx_id`    an integer sequnce id for the transaction
* `valid_time`  a tuple of `{timestamp, EpocMillis}`

Where EpochMillis is the number of milliseconds elapsed since January 1, 1970, 00:00:00 GMT.

Example:
```erlang
%% Add a new person, the system reports back tx_id and tx_time
> xt:put(#person{person_id="time1", first_name="Time", last_name="Traveler"}).
{ok,{37,{timestamp,1670863315580}}}

%% Querying at the same tx will find it
> xt:ql(#person{person_id="time1"},[{tx_id,37}]).
[#person{person_id = "time1",first_name = "Time",
         last_name = "Traveler",email = undefined,
         date_of_birth = undefined,shipping_address = undefined,
         billing_address = undefined}]

%% Querying at any time before the tx it was added, doesn't find it
> xt:ql(#person{person_id="time1"},[{tx_id,36}]).
[]

%% Put new version of document
> xt:put(#person{person_id="time1", first_name="Chrono", last_name="Jumper"}).
{ok,{38,{timestamp,1670863377972}}}

%% Previous is still present in history
> xt:ql(#person{person_id="time1"},[{tx_id,37}]).
[#person{person_id = "time1",first_name = "Time",
         last_name = "Traveler",email = undefined,
         date_of_birth = undefined,shipping_address = undefined,
         billing_address = undefined}]

%% New is the latest one
> xt:ql(#person{person_id="time1"},[{tx_id,38}]).
[#person{person_id = "time1",first_name = "Chrono",
         last_name = "Jumper",email = undefined,
         date_of_birth = undefined,shipping_address = undefined,
         billing_address = undefined}]
```

You can use either `tx_time` or `tx_id` to query. Not that you can't request a
tx time that is in the future (the node will report that it isn't synced up to
that point in time). You can query with a `valid_time` in the future.

## Build the library

    $ rebar3 compile

## Run the XTDB node

The `xtdb` folder contains the Clojure project for running the database node.
The database node acts as an Erlang named process that accepts queries and commands.

## TODO

Here are some planned features:

* ~~Partial fetch (pull only some fields)~~
* Query ids and pull by ids (for doing fine grained pagination things)
* Batching results (send results back by messages of wanted size)
* Mapping for linked documents (query and pull them)
* ~~Batching multiple write operations~~
* ~~Time travel (options to set tx time and valid time)~~
* `put` options for valid_time
* `delete` tx
* `match` tx
* `evict` tx
