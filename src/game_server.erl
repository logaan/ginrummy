-module(game_server).
-behaviour(gen_server).
-include("records.hrl").

%% API
-export([start/1, start/2, library_draw/2, discard_draw/2, discard/3, manual_sort/3, value_sort/2, state/1, stop/0]).

%% gen-server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================
start(State = #game{ game_name=GameName }) ->
  Result = gen_server:start_link({local, GameName}, ?MODULE, State, []),
  io:format("Starting Result: ~p~n", [Result]),
  {GameName, Result}.
start(Player1Name, Player2Name) ->
  {new_id, Id} = id_server:new_id(),
  GameName = list_to_atom(lists:concat(["g", Id])),
  {GameName, gen_server:start_link({local, GameName}, ?MODULE, [Player1Name, Player2Name, GameName], [])}.
library_draw(Game, Player) ->
  gen_server_call(Game, {library_draw, Player}).
discard_draw(Game, Player) ->
  gen_server_call(Game, {discard_draw, Player}).
discard(Game, Player, CardName) ->
  gen_server_call(Game, {discard, Player, CardName}).
manual_sort(Game, PlayerNumber, NewOrder) ->
  gen_server_call(Game, {manual_sort, PlayerNumber, NewOrder}).
value_sort(PlayerNumber, Game) ->
  gen_server_call(Game, {value_sort, PlayerNumber}).
state(Game) ->
  gen_server_call(Game, state).
stop() ->
  gen_server_call(?MODULE, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Player1Name, Player2Name, GameName]) ->
  {ok, game:start_game(Player1Name, Player2Name, GameName)};
init(State) when is_record(State, game) ->
  {ok, ChatServer} = chat_server:start_link(),
  chat_server:subscribe(1, ChatServer),
  chat_server:subscribe(2, ChatServer),
  {ok, State#game{ chat_server=ChatServer }}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, State) ->
  io:format("restarting after crash~n"),
  restarter:on_exit(fun(_,_) -> game_server:start(State) end, self()),
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_call(stop, _From, State) -> {stop, normal, State };

handle_call({library_draw, Player}, _From, State) ->
  case game:library_draw(Player, State) of
    {error, ErrorMessage} ->
      chat_server:direct_message(Player, {chat, ErrorMessage}, State#game.chat_server),
      {reply, {library_draw, State}, State};
    NewState ->
      Message = lists:concat([player_name(State, Player), " drew a card"]),
      chat_server:broadcast({chat, Message}, NewState#game.chat_server),
      {reply, {library_draw, NewState}, NewState}
  end;

handle_call({discard_draw, Player}, _From, State) ->
  case game:discard_draw(Player, State) of
    {error, ErrorMessage} ->
      chat_server:direct_message(Player, {chat, ErrorMessage}, State#game.chat_server),
      {reply, {discard_draw, State}, State};
    NewState ->
      Message = lists:concat([player_name(State, Player), " drew from the discard pile"]),
      chat_server:broadcast({chat, Message}, NewState#game.chat_server),
      {reply, {discard_draw, NewState}, NewState}
  end;

handle_call({discard, Player, CardName}, _From, State) ->
  NewState = game:discard(Player, CardName, State),
  Message = lists:concat([player_name(State, Player), " discarded ", CardName]),
  chat_server:broadcast({chat, Message}, NewState#game.chat_server),
  {reply, {discard, NewState}, NewState};

handle_call({manual_sort, PlayerNumber, NewOrder}, _From, State) ->
  NewState = game:manual_sort(PlayerNumber, NewOrder, State),
  chat_server:refresh(PlayerNumber, NewState#game.chat_server),
  {reply, {manual_sort, NewState}, NewState};

handle_call({value_sort, PlayerNumber}, _From, State) ->
  NewState = game:value_sort(PlayerNumber, State),
  chat_server:refresh(PlayerNumber, NewState#game.chat_server),
  {reply, {value_sort, NewState}, NewState};

handle_call(crash, _From, _State) ->
  1/0;

handle_call(state, _From, State) ->
  {reply, {state, State}, State}.

%%====================================================================
%%% Internal functions
%%====================================================================
player_name(#game{ players=Players }, Number) ->
  Player = lists:nth(Number, Players),
  Player#player.name.

gen_server_call(Pid, Message) when is_list(Pid) ->
  AtomicPid = list_to_atom(Pid),
  gen_server:call(AtomicPid, Message);
gen_server_call(Pid, Message) ->
  gen_server:call(Pid, Message).

%%====================================================================
%%% Unit tests
%%====================================================================

