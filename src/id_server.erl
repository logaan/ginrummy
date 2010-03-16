-module(id_server).
-behaviour(gen_server).

%% API
-export([start/0, state/0, new_id/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================
start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

state() ->
  gen_server:call(?MODULE, state).

new_id() ->
  gen_server:call(?MODULE, new_id).

stop() ->
  gen_server:call(?MODULE, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
  {ok, 0}.

handle_call(new_id, _From, State) ->
  NewId = State + 1,
  Reply = {new_id, NewId},
  {reply, Reply, NewId};
handle_call(state, _From, State) ->
  Reply = State,
  {reply, Reply, State};
handle_call(stop, _From, State) ->
  {stop, normal, State };
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%====================================================================
%%% Internal functions
%%====================================================================

%%====================================================================
%%% Unit tests
%%====================================================================

