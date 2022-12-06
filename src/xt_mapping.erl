%% Mapping from records to XTDB documents
-module(xt_mapping).
-export([to_doc/2, from_doc/2]).
-include("xt_mapping.hrl").

to_doc(Record, Mapping) ->
    lists:foldl(fun({Name,Field}, Doc) -> [{Name, element(Field, Record)} | Doc];
                   ({_Name,_,Write}, Doc) -> Write(Record, Doc)
                end,
                [], Mapping).

id(Field,
