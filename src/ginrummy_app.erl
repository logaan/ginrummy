-module(ginrummy_app).

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for ginrummy.
start(_Type, _StartArgs) ->
    id_server:start_link(),
    ginrummy_deps:ensure(),
    ginrummy_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for ginrummy.
stop(_State) ->
    id_server:stop(),
    ok.
