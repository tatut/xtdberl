%% Contains records and other type definitions for XTDBerl 

-type timestamp() :: {timestamp, non_neg_integer()}.

-record(txinfo, {%% Tx sequence number
                 tx_id :: non_neg_integer(),

                 %% Tx time (server time in milliseconds)
                 tx_time :: timestamp()}).

%% An orddict or map can be made into a doc
-type doclike() :: [{atom(), any()}] | #{atom() => any()}.
