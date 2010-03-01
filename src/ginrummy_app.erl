-module(ginrummy_app).

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for ginrummy.
start(_Type, _StartArgs) ->
    ginrummy_deps:ensure(),

    gin_rummy:setup_database(),
    lists:map(fun gin_rummy:insert_card/1, gin_rummy:generate_playing_cards()),
    nitrogen:start(gin_rummy),

    ginrummy_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for ginrummy.
stop(_State) ->
    nitrogen:stop(),
    gin_rummy:teardown_database(),
    ok.
