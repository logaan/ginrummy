-module(game_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2, code_change/3]).
-export([start/2, stop/0]).
-include("records.hrl").

init([Player1Name, Player2Name]) ->
  {ok, game:start_game(Player1Name, Player2Name)}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_call(stop, _From, State) ->
  {stop, normal, State };

handle_call({library_draw, Player}, _From, State) ->
  NewState = game:library_draw(Player, State),
  chat_server:broadcast("Someone drew a card", State#game.chat_server),
  {reply, {library_draw, NewState}, NewState};

handle_call({discard_draw, Player}, _From, State) ->
  NewState = game:discard_draw(Player, State),
  {reply, {discard_draw, NewState}, NewState};

handle_call({discard, Player, CardName}, _From, State) ->
  NewState = game:discard(Player, CardName, State),
  {reply, {discard, NewState}, NewState};

handle_call(game_state, _From, State) ->
  {reply, {game_state, State}, State}.

start(Player1Name, Player2Name) ->
  GameName = list_to_atom(uuid:to_string(uuid:v4())),
  {GameName, gen_server:start_link({local, GameName}, ?MODULE, [Player1Name, Player2Name], [])}.

stop() ->
  gen_server:call(?MODULE, stop).

