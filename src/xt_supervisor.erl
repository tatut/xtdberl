-module(xt_supervisor).
-behaviour(supervisor).
-export([start_link/1, init/1]).

start_link(Args) ->
    supervisor:start_link(xt_supervisor, Args).

init(Args) ->
    {ok, {#{strategy => one_for_one},
          [#{id => xt,
             start => {xt, start_link, [Args]}}]}}.
