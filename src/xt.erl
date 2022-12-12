%% @doc The main application and API namespace for using XTDB.
-module(xt).
-export([start/2, stop/1, init/1, monitor_xtdb/2,
         register_xtdb_node/1,
         q/3, q/2, put/1, status/0, ql/1, ql/2]).
-include("types.hrl").

%%%%%%%%%%%
% Application lifecycle functions

start(Type, Args) ->
    io:format("START ~p ~p~n", [Type,Args]),
    Nodes = orddict:fetch(xtdb_nodes,Args),
    logger:info("Starting XTDB interface with ~p node(s)",[length(Nodes)]),
    Pid = spawn(?MODULE, init, [Nodes]),
    register(xtdb, Pid),
    {ok, Pid}.

stop(X) ->
    logger:info("Stopping XTDB interface ~p~n", [X]),
    ok.

init(Nodes) ->
    %% Spawn a process that monitors each node
    Me = self(),
    lists:foreach(fun(N) -> spawn(?MODULE, monitor_xtdb, [Me, N]) end, Nodes),
    loop([]).

%% @doc Add a new XTDB node
-spec register_xtdb_node({atom(), atom()}) -> ok.
register_xtdb_node(Node) ->
    Pid = whereis(xtdb),
    spawn(?MODULE, monitor_xtdb, monitor_xtdb, [Pid, Node]).

monitor_xtdb(Parent, Node) ->
    logger:debug("Monitoring XTDB node: ~p for parent ~p~n", [Node,Parent]),
    monitor_xtdb(1000, Parent, Node, undefined).

%% Monitor an XTDB jinterface node availability by pinging it.
%% Notifies the application process when nodes are up/down so it
%% can keep track of what nodes are available.
monitor_xtdb(Wait, Parent, {_, Node}=N, PreviousStatus) ->
    timer:sleep(Wait),
    NewStatus = net_adm:ping(Node),
    case {PreviousStatus, NewStatus} of
        {undefined, pong} -> Parent ! {xtdb_up, N};
        {undefined, pang} -> Parent ! {xtdb_down, N};
        {pang, pong} -> Parent ! {xtdb_up, N};
        {pong, pang} -> Parent ! {xtdb_down, N};
        {pang, pang} -> still_down;
        {pong, pong} -> still_up
    end,
    monitor_xtdb(5000, Parent, N, NewStatus).

loop(AvailableNodes) ->
    receive
        {get_node, From} ->
            case AvailableNodes of
                [] -> From ! no_available_xtdb_node;
                [N|_] -> From ! {xtdb_node, N}
            end;
        {xtdb_up, Node} ->
            %% Add node to available
            logger:info("XTDB node up: ~p", [Node]),
            NewNodes = [Node|AvailableNodes],
            if length(NewNodes) == 1 ->
                    logger:notice("1 XTDB node is now available.");
               true -> ok
            end,
            loop(NewNodes);
        {xtdb_down, Node} ->
            %% Remove node from available
            logger:info("XTDB node down: ~p", [Node]),
            NewNodes = lists:delete(Node, AvailableNodes),
            if length(NewNodes) == 0 ->
                    logger:alert("No XTDB nodes available, subsequent queries and transactions will fail!");
               true -> ok
            end,
            loop(NewNodes);
        Else ->
            logger:warning("Unrecognized message received: ~p", [Else])
    end,
    loop(AvailableNodes).


pid() ->
    xtdb ! {get_node, self()},
    receive
        {xtdb_node, Node} -> Node;
        no_available_xtdb_node -> throw(no_available_xtdb_node)
    end.

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
    ql(Candidate,[]).

-spec ql(tuple(), [{atom(),any()}]) -> [tuple()].
%% @doc Query Like record instances with options.
%% The supported options are:
%% <dl>
%%   <dt>mapping</dt><dd>the record mapping to use</dd>
%% </dl>
%% @see xt_mapping:qlike/2.
ql(Candidate,Options) when is_tuple(Candidate) ->
    RecordType = element(1,Candidate),
    Mapping = case orddict:find(mapping, Options) of
                  {ok, M} -> M;
                  error -> xt_mapping:get(RecordType)
              end,
    {Query,Where,In} = xt_mapping:qlike(Candidate, Mapping, Options),
    case q(Query,Where,In) of
        {ok, Results} -> xt_mapping:read_results(Results, Mapping);
        timeout -> timeout
    end.

%% project with fetch
%%ql(#person{first_name={'>', "A"}},
%%   [{fetch, [#person.first_name, #person.last_name,
%%             {#person.address, [#address.country]}]}]
