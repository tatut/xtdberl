-module(xt_app).
-export([start/2, stop/1]).

%% Application lifecycle for xt

start(_, Args) -> xt_supervisor:start_link(Args).
stop(_) ->
    % PENDING: do we need to do anything?
    ok.
