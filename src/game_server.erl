-module(game_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2, code_change/3]).
-export([start/2, stop/0]).

init([Player1Name, Player2Name]) ->
  {ok, gin_rummy:start_game(Player1Name, Player2Name)}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_call(stop, _From, State) ->
  {stop, normal, State };
 
handle_call(game_state, _From, State) ->
    {reply, {game_state, State}, State}.

start(Player1Name, Player2Name) ->
  GameName = list_to_atom(string:join([io_lib:print(?MODULE), Player1Name, Player2Name], "_")),
  gen_server:start_link({local, GameName}, ?MODULE, [Player1Name, Player2Name], []).

stop() ->
  gen_server:call(?MODULE, stop).

