%% Contains records and other type definitions for XTDBerl

%% @type timestamp in Java milliseconds from epoch
-type timestamp() :: {timestamp, non_neg_integer()}.

%% @type Transaction information returned by XTDB.
%% The tx_id is the increasing transaction number and
%% the tx_time is the server transaction time.
-record(txinfo, {%% Tx sequence number
                 tx_id :: non_neg_integer(),

                 %% Tx time (server time in milliseconds)
                 tx_time :: timestamp()}).

%% @type A record (must have mapping) or a key/value map
-type doclike() :: tuple() | #{atom() => any()}.
