%% @doc The main application and API namespace for using XTDB.
-module(xt).
-export([start_link/1, stop/1, init/1, monitor_xtdb/2,
         register_xtdb_node/1,
         q/1, put/1, status/0, ql/1, ql/2, qlcount/1, qlcount/2,
         batch/1, batch_handler/2
        ]).
-include("types.hrl").

%%%%%%%%%%%
% Application lifecycle functions

start_link(Args) ->
    {xtdb_nodes, Nodes} = lists:keyfind(xtdb_nodes, 1, Args),
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
    loop([], #{}).

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

%% PENDING: turn this into a gen_server
loop(AvailableNodes, Batches) ->
    receive
        {get_node, From} ->
            case AvailableNodes of
                [] -> From ! no_available_xtdb_node;
                [N|_] -> From ! {xtdb_node, N}
            end,
            loop(AvailableNodes, Batches);

        {get_node_or_batch, From} ->
            case maps:get(From, Batches, no_batch) of
                no_batch -> case AvailableNodes of
                                [] -> From ! no_available_xtdb_node;
                                [N|_] -> From ! {xtdb_node, N}
                            end;
                Batch -> From ! {xtdb_node, Batch}
            end,
            loop(AvailableNodes, Batches);

        {batch_start, From} ->
            Pid = spawn_link(?MODULE, batch_handler, [From,[]]),
            From ! {batch, Pid},
            loop(AvailableNodes, maps:put(From, Pid, Batches));

        {batch_done, From} ->
            loop(AvailableNodes, maps:remove(From, Batches));

        {xtdb_up, Node} ->
            %% Add node to available
            logger:info("XTDB node up: ~p", [Node]),
            NewNodes = [Node|AvailableNodes],
            if length(NewNodes) == 1 ->
                    logger:notice("1 XTDB node is now available.");
               true -> ok
            end,
            loop(NewNodes, Batches);

        {xtdb_down, Node} ->
            %% Remove node from available
            logger:info("XTDB node down: ~p", [Node]),
            NewNodes = lists:delete(Node, AvailableNodes),
            if length(NewNodes) == 0 ->
                    logger:alert("No XTDB nodes available, subsequent queries and transactions will fail!");
               true -> ok
            end,
            loop(NewNodes, Batches);

        Else ->
            %% FIXME: handle batch dying
            logger:warning("Unrecognized message received: ~p", [Else]),
            loop(AvailableNodes,Batches)
    end.




pid() ->
    xtdb ! {get_node, self()},
    receive
        {xtdb_node, Node} -> Node;
        no_available_xtdb_node -> throw(no_available_xtdb_node)
    end.

%% Returns pid of XTDB process, or a local batch process
%% if a write batching is in operation
pid_or_batch() ->
    xtdb ! {get_node_or_batch, self()},
    receive
        {xtdb_node, Node} -> Node;
        {batch, Batch} -> Batch;
        no_available_xtdb_node -> throw(no_available_xtdb_node)
    end.

supported_q_option({tx_time,_}) -> true;
supported_q_option({valid_time,_}) -> true;
supported_q_option({tx_id,_}) -> true;
supported_q_option({listen,_}) -> true;
supported_q_option({defer,_}) -> true;
supported_q_option(_) -> false.

build_opts({Key,Val}=Opt, O) ->
    case supported_q_option(Opt) of
        true -> maps:put(Key, Val, O);
        false -> O
    end.

-spec build_q(tuple(), #{}) -> #{}.
build_q({find, Find}, Q) ->
    maps:put(':find', Find, Q);
build_q({where, Where}, Q) ->
    maps:put(':where', Where, Q);
build_q({in, In}, Q) ->
    maps:put(':in', [K || {K,_} <- In], Q);
build_q({limit, N}, Q) ->
    maps:put(':limit', N, Q);
build_q({offset, N}, Q) ->
    maps:put(':offset', N, Q);
build_q({order_by, FieldsAndDirections}, Q) ->
    maps:put(':order-by', FieldsAndDirections, Q);
build_q({defer,_}, Q) -> Q;
build_q({read_results,_}, Q) -> Q;
build_q({listen,_}, Q) -> Q;
build_q(Option, Q) ->
    case supported_q_option(Option) of
        %% Skip any other supported query options
        %% that are not a part of the query map itself
        true -> Q;
        false -> throw({unrecognized_query_option, Option})
    end.

build_args(Options) ->
    {in, In} = lists:keyfind(in, 1, Options),
    [V || {_,V} <- In].


ensure_args(Opts) ->
    case lists:keyfind(in, 1, Opts) of
        false -> [{in,[]} | Opts];
        {in,_} -> Opts
    end.

-spec q([tuple()]) -> [any()]. % more detailed spec
q(QueryAndOptions0) ->
    QueryAndOptions = ensure_args(QueryAndOptions0),
    Query = lists:foldl(fun build_q/2, #{}, QueryAndOptions),
    Opts = lists:foldl(fun build_opts/2, #{}, QueryAndOptions),
    Args = build_args(QueryAndOptions),
    %%io:format("QUERY: ~p, HAS ARGS: ~p~n", [Query,Args]),
    QueryId = make_ref(),

    ReadResults = case lists:keyfind(read_results, 1, QueryAndOptions) of
                      {_, Fun} when is_function(Fun) -> Fun;
                      false -> fun(R) -> R end
                  end,
    case lists:keyfind(defer, 1, QueryAndOptions) of
        %% Defer query result, send it to another pid instead
        {defer, DeferToPid} ->
            spawn(fun() ->
                          pid() ! {q, self(), QueryId, Opts, Query, Args},
                          Listen = lists:keymember(listen, 1, QueryAndOptions),

                          %% Link the process that wants to receive results and trap exit
                          link(DeferToPid),
                          process_flag(trap_exit, true),

                          defer_loop(DeferToPid, QueryId, ReadResults, Listen)
                  end),
            {deferred, QueryId};

        %% Send query and receive query result to self
        false ->
            %% Send the query
            pid() ! {q, self(), QueryId, Opts, Query, Args},
            receive
                {ok, QueryId, Results} -> ReadResults(Results);
                {error, QueryId, ErrorInfo} -> throw(ErrorInfo);
                Msg -> io:format("Received something else: ~p~n", [Msg]), Msg
            after 30000 -> %% configure as options
                    timeout
            end
    end.

defer_loop(Pid, QueryId, ReadResults, Listen) ->
    receive
        {ok, QueryId, Results} ->
            Pid ! {ok, QueryId, ReadResults(Results)},
            case Listen of
                false -> done;
                true -> defer_loop(Pid, QueryId, ReadResults, Listen)
            end;
        {error, QueryId, ErrorInfo} ->
            Pid ! {error, QueryId, ErrorInfo};

        %% Listener process has exited, we can stop as well.
        %% The listener mbox on the XTDB node will be notified.
        {'EXIT', Pid, Why} ->
            logger:debug("Listening PID ~p has exited: ~p", [Pid, Why])
    end.


-spec put(doclike() | [doclike()]) -> {ok, #txinfo{}} | {error, any()}.

doclist([],Acc) -> Acc;
doclist([D|Docs], Acc) ->
    doclist(Docs, [doc(D) | Acc]);
doclist(X,[]) -> [doc(X)].

doc(Record) when is_tuple(Record) ->
    %% Erlang record tuple
    xt_mapping:to_doc(Record, xt_mapping:get(Record));
doc(#{'__struct__' := _} = Struct) ->
    %% Elixir struct map
    xt_mapping:to_doc(Struct, xt_mapping:get(Struct));
doc(Map) when is_map(Map) -> Map.

%% @doc Put a new document into the database.
%% Doc can be a record that has an installed mapping or a
%% raw key/value document map.
%% If document is a list, then multiple documents are put.
put(Doc) ->
    MsgId = make_ref(),
    pid_or_batch() ! {put, self(), MsgId, doclist(Doc,[])},
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

get_mapping(Candidate, Options) ->
    case lists:keyfind(mapping, 1, Options) of
        false -> xt_mapping:get(Candidate);
        {mapping, M} -> M
    end.

%% @doc Query Like record instances.
%% Takes a candidate instance and creates a query to find similar instances.
%% Requires that a record mapping is registered beforehand.
%% @see xt_mapping:register/1.
%% @see xt_mapping:qlike/2.
ql(Candidate) when is_tuple(Candidate) orelse is_map(Candidate) ->
    ql(Candidate,[]).

-spec ql(tuple() | map(), [{atom(),any()}]) -> [tuple()].
%% @doc Query Like record instances with options.
%% The supported options are:
%% <dl>
%%   <dt>mapping</dt><dd>the record mapping to use</dd>
%% </dl>
%% @see xt_mapping:qlike/2.
ql(Candidate,Options) when is_tuple(Candidate) orelse is_map(Candidate) ->
    Mapping = get_mapping(Candidate, Options),
    q([{read_results,
        fun(R) -> xt_mapping:read_results(R,Mapping) end} |
       xt_mapping:qlike(Candidate, Mapping, Options)]).

qlcount(Candidate) ->
    qlcount(Candidate,[]).
qlcount(Candidate, Options) ->
    Mapping = get_mapping(Candidate, Options),
    q([{read_results, fun([[C]]) -> C end} |
       xt_mapping:qlike_count(Candidate, Mapping, Options)]).



%% Batch multiple write operations into a single XTDB transaction.
%% The transaction is sent once the function returns if any operations
%% were issued by it.
batch(Fun) ->
    xtdb ! {batch_start, self()},
    %%io:format("waiting for batch process pid~n", []),
    receive
        {batch, BatchPid} ->
            %%io:format("got batch process pid ~p ~n", [BatchPid]),
            Fun(),
            BatchPid ! {execute, self()},
            %%io:format("sent batch execute, waiting for results",[]),
            receive
                {batch_result, Res} -> {ok, Res}
            end
    after 5000 ->
            logger:warning("Could not start batch withing 5 seconds."),
            throw(batch_start_timeout)
    end.



batch_handler(From, Operations) ->
    receive
        {execute, ResultPid} ->
            BatchId = make_ref(),

            %%io:format("Executing batch with ~p operation~n", [length(Operations)]),

            %% Tell XTDB that we are done with the batch
            xtdb ! {batch_done, From},

            %% Ask for the XTDB node, send the batch to it
            xtdb ! {get_node, self()},
            receive
                {xtdb_node, XtdbPid} ->
                    %%io:format("got XTDB node ~p, sending ~n", [XtdbPid]),
                    XtdbPid ! {batch, self(), BatchId,
                               lists:reverse(Operations)}
            end,

            %% Wait for batch TX acknowldgement
            receive
                {ok, BatchId, Results} ->
                    %%io:format("received result from xtdb ~p~n", [Results]),
                    ResultPid ! {batch_result, Results}
            end;
        {put, From, Id, Payload} ->
            From ! {ok, Id, batched},
            batch_handler(From, [{put, Payload}|Operations])
    end.




%% project with fetch
%%ql(#person{first_name={'>', "A"}},
%%   [{fetch, [#person.first_name, #person.last_name,
%%             {#person.address, [#address.country]}]}]
