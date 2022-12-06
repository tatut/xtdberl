-module(xt).
-export([q/3, q/2, put/1, demo/0]).
-include("types.hrl").
%% Convenient interface to XTDB process

%% FIXME: configure where the xtdb is located
pid() -> {xtdb, xtdb@localhost}.

q(Find, Where) -> q(Find,Where,[]).
q(Find, Where, In) ->
    QueryId = make_ref(),
    pid() ! {q, self(), QueryId,
             ([':find' | Find] ++ [':where' | Where] ++
                 (case length(In) of
                      0 -> [];
                      _ -> [':in' | [K || {K,_} <- In]]
                  end)),
             [V || {_,V} <- In]},

    receive
        {ok, QueryId, Results} -> {ok, Results};
        {error, QueryId, ErrorInfo} -> throw(ErrorInfo);
        Msg -> io:format("Received something else: ~p~n", [Msg]), Msg
    after 30000 -> %% configure as options
            timeout
    end.

-spec put(doclike()) -> {ok, #txinfo{}} | {error, any()}.


put(Doc) ->
    MsgId = make_ref(),
    pid() ! {put, self(), MsgId, Doc},
    receive
        {ok, MsgId, TxInfo} ->
            {ok, TxInfo};
        {error, MsgId, Error} ->
            {error, Error}
    after 5000 -> timeout
    end.

demo() ->
    Result = q([x,jotain], %% :find clause
               [[x, ':jotain', jotain],
                [x, ':name', foo]],  %% :where clauses
               [{foo, "hep"}] %% :in parameters as an orddict
              ),
    io:format("Got result: ~p~n", [Result]),
    Result.
