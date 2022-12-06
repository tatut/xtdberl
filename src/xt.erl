-module(xt).
-export([q/3, q/2, demo/0]).
%% Convenient interface to XTDB process

%% FIXME: configure where the xtdb is located
pid() -> {xtdb, xtdb@localhost}.

q(Find, Where) -> q(Find,Where,[]).
q(Find, Where, In) ->
    QueryId = make_ref(),
    pid() ! {q, self(), make_ref(),
             ([':find' | Find] ++ [':where' | Where] ++
                 (case length(In) of
                      0 -> [];
                      _ -> [':in' | [K || {K,_} <- In]]
                  end)),
             [V || {_,V} <- In]},
    receive
        {ok, QueryId, Results} -> {ok, Results};
        {error, ErrorInfo} -> throw(ErrorInfo)
    after 5000 ->
            timeout

    end.

demo() ->
    q([x], %% :find clause
      [[x, ':name', foo]],  %% :where clauses
      [{foo, "hep"}] %% :in parameters as an orddict
     ).
