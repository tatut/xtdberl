%% @doc The main API namespace for using XTDB.
-module(xt).
-export([q/3, q/2, put/1, status/0, ql/1, ql/2]).
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

-spec put(doclike() | [doclike()]) -> {ok, #txinfo{}} | {error, any()}.

doclist([],Acc) -> Acc;
doclist([D|Docs], Acc) ->
    doclist(Docs, [doc(D) | Acc]);
doclist(X,[]) -> [doc(X)].

doc(Record) when is_tuple(Record) ->
    xt_mapping:to_doc(Record,
                      xt_mapping:get(element(1,Record)));
doc(Map) when is_map(Map) -> Map.

%% @doc Put a new document into the database.
%% Doc can be a record that has an installed mapping or a
%% raw key/value document map.
%% If document is a list, then multiple documents are put.
put(Doc) ->
    MsgId = make_ref(),
    pid() ! list_to_tuple([put, self(), MsgId] ++ doclist(Doc,[])),
    receive
        {ok, MsgId, TxInfo} ->
            {ok, TxInfo};
        {error, MsgId, Error} ->
            {error, Error}
    after 5000 -> timeout
    end.

-spec status() -> map().
%% @doc Return XTDB node status information.
status() ->
    MsgId = make_ref(),
    pid() ! {status, self(), MsgId},
    receive
        {ok, MsgId, Status} ->
            {ok, Status}
    after 5000 -> timeout
    end.

%% @doc Query Like record instances.
%% Takes a candidate instance and creates a query to find similar instances.
%% Requires that a record mapping is registered beforehand.
%% @see xt_mapping:register/1.
%% @see xt_mapping:qlike/2.
ql(Candidate) when is_tuple(Candidate) ->
    RecordType = element(1, Candidate),
    ql(Candidate,[{mapping, xt_mapping:get(RecordType)}]).

-spec ql(tuple(), [{atom(),any()}]) -> [tuple()].
%% @doc Query Like record instances with options.
%% The supported options are:
%% <dl>
%%   <dt>mapping</dt><dd>the record mapping to use</dd>
%% </dl>
%% @see xt_mapping:qlike/2.
ql(Candidate,Options) when is_tuple(Candidate) ->
    Mapping = orddict:fetch(mapping, Options),
    {Query,Where,In} = xt_mapping:qlike(Candidate, Mapping),
    {ok, Results} = q(Query,Where,In),
    xt_mapping:read_results(Results, Mapping).
