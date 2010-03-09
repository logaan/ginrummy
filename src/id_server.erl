-module(id_server).
-behaviour(gen_server).

%% API
-export([start_link/0, state/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

% Records
-record(state, {}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
  gen_server:start_link(?MODULE, [], []).

state(Pid) ->
  gen_server:call(Pid, state).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
  {ok, #state{}}.

handle_call(state, _From, State) ->
  Reply = State,
  {reply, Reply, State};
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

