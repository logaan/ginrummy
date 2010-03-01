-module(ginrummy).
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the ginrummy server.
start() ->
    ginrummy_deps:ensure(),
    ensure_started(crypto),
    application:start(ginrummy).

%% @spec stop() -> ok
%% @doc Stop the ginrummy server.
stop() ->
    Res = application:stop(ginrummy),
    application:stop(crypto),
    Res.
