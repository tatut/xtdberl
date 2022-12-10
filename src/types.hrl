%% Contains records and other type definitions for XTDBerl

-type timestamp() :: {timestamp, non_neg_integer()}.

-record(txinfo, {%% Tx sequence number
                 tx_id :: non_neg_integer(),

                 %% Tx time (server time in milliseconds)
                 tx_time :: timestamp()}).

%% A record (must have mapping) or a key/value map
-type doclike() :: tuple() | #{atom() => any()}.
