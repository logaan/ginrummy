-module(game_controller,[Env]).
-export([handle_request/2]).
-include("records.hrl").

handle_request("start",[]) ->
  PlayerOneName       = beepbeep_args:get_param("player_one_name",Env),
  PlayerTwoName       = beepbeep_args:get_param("player_two_name",Env),
  {AtomicGameName, _} = game_server:start(PlayerOneName, PlayerTwoName),
  GameName            = atom_to_list(AtomicGameName),
  {state, Game}       = game_server:state(GameName),
  beepbeep_args:set_session_data(GameName, 1, Env),
  chat_server:subscribe(1, Game#game.chat_server),
  chat_server:subscribe(2, Game#game.chat_server),
  {redirect, lists:concat(["/game/", GameName])};

handle_request(GameName, []) ->
  {state, Game}          = game_server:state(GameName),
  [PlayerOne, PlayerTwo] = Game#game.players,

  ViewData = case beepbeep_args:get_session_data(GameName, Env) of
    1 -> html_view_data(Game, PlayerOne, PlayerTwo);
    2 -> html_view_data(Game, PlayerTwo, PlayerOne);
    undefined ->
      beepbeep_args:set_session_data(GameName, 2, Env),
      Message = lists:concat([PlayerTwo#player.name, " joined the game"]),
      chat_server:broadcast({chat, Message}, Game#game.chat_server),
      html_view_data(Game, PlayerTwo, PlayerOne)
  end,
  {render, "game/show.html", ViewData};

handle_request(GameName, ["library_draw"]) ->
  PlayerNumber      = beepbeep_args:get_session_data(GameName, Env),
  {library_draw, _} = game_server:library_draw(GameName, PlayerNumber),
  ajax_response(GameName);

handle_request(GameName, ["discard_draw"]) ->
  PlayerNumber      = beepbeep_args:get_session_data(GameName, Env),
  {discard_draw, _} = game_server:discard_draw(GameName, PlayerNumber),
  ajax_response(GameName);

handle_request(GameName, ["discard", CardName]) ->
  PlayerNumber   = beepbeep_args:get_session_data(GameName, Env),
  {discard, _}   = game_server:discard(GameName, PlayerNumber, CardName),
  ajax_response(GameName);

handle_request(GameName, ["knock"]) ->
  PlayerNumber         = beepbeep_args:get_session_data(GameName, Env),
  OpponentNumber       = if PlayerNumber == 1 -> 2; true -> 1 end,
  {state, Game}        = game_server:state(GameName),
  #player{ hand=Hand } = game:get_player(PlayerNumber, Game),
  ChatServer           = game:chat_server(Game),
  chat_server:direct_message(OpponentNumber, {knock, Hand}, ChatServer),
  ajax_response(GameName);

handle_request(GameName, ["comet"]) ->
  % If the server goes down then so should the comet so that the client can
  % re-connect to the restarted server
  link(whereis(list_to_atom(GameName))),
  PlayerNumber       = beepbeep_args:get_session_data(GameName, Env),
  {state, Game}      = game_server:state(GameName),
  chat_server:listen(PlayerNumber, self(), Game#game.chat_server),
  receive
    {messages, Messages} ->
      chat_server:unlisten(PlayerNumber, self(), Game#game.chat_server),
      {state, NewGame} = game_server:state(GameName),
      {render, "game/comet.html", json_view_data(NewGame, PlayerNumber, Messages)}
  end;

handle_request(GameName, ["broadcast"]) ->
  PlayerNumber       = beepbeep_args:get_session_data(GameName, Env),
  {state, Game}      = game_server:state(GameName),
  Message            = beepbeep_args:get_param("message", Env),
  Player             = lists:nth(PlayerNumber, Game#game.players),
  FormattedMessage   = lists:concat([Player#player.name, " : ", Message]),
  chat_server:broadcast({chat, FormattedMessage}, Game#game.chat_server),
  ajax_response(GameName);

handle_request(GameName, ["manual_sort"]) ->
  PlayerNumber    = beepbeep_args:get_session_data(GameName, Env),
  BinaryCardNames = mochijson2:decode(beepbeep_args:get_param("card_names", Env)),
  StringCardNames = lists:map(fun binary_to_list/1, BinaryCardNames),
  game_server:manual_sort(GameName, PlayerNumber, StringCardNames),
  ajax_response(GameName);

handle_request(GameName, ["value_sort"]) ->
  PlayerNumber = beepbeep_args:get_session_data(GameName, Env),
  game_server:value_sort(PlayerNumber, GameName),
  ajax_response(GameName).

html_view_data(#game{zones=Zones}, CurrentPlayer, Opponent) ->
  Deck    = proplists:get_value(deck, Zones),
  Discard = proplists:get_value(discard, Zones),
  [
    {game_name,       beepbeep_args:get_action(Env)},
    {player_one_name, CurrentPlayer#player.name},
    {player_two_name, Opponent#player.name},
    {card_count,      length(CurrentPlayer#player.hand)},
    {your_hand,       card_list(CurrentPlayer)},
    {top_of_discard,  top_of_discard(Discard)},
    {deck_size,       length(Deck)},
    {opponent_size,   length(Opponent#player.hand)}
  ].

json_view_data(#game{ players=[PlayerOne, PlayerTwo], zones=Zones }, PlayerNumber, Messages) ->
  Deck    = proplists:get_value(deck, Zones),
  Discard = proplists:get_value(discard, Zones),
  case PlayerNumber of
    1 -> Player = PlayerOne, Opponent = PlayerTwo;
    2 -> Player = PlayerTwo, Opponent = PlayerOne
  end,
  ChatMessages = [ list_to_binary(M) || {chat, M} <- Messages],
  OpponentHand = case lists:keysearch(knock, 1, Messages) of
    {value, {knock, Hand}} ->
      lists:map(fun(#card{ name=Name }) -> list_to_binary(Name) end, Hand);
    false ->
      []
  end,
  Data = {struct, [
    {"player_size",     length(Player#player.hand)},
    {"player_hand",     card_list(Player)},
    {"opponent_size",   length(Opponent#player.hand)},
    {"deck_size",       length(Deck)},
    {"top_of_discard",  list_to_binary(top_of_discard(Discard))},
    {"new_messages",    ChatMessages},
    {"opponent_hand",   OpponentHand}
  ]},
  [ {json, iolist_to_binary(mochijson2:encode(Data))} ] .

card_list(Player) ->
  Hand = Player#player.hand,
  lists:map(fun(Card) -> list_to_binary(Card#card.name) end, Hand).

top_of_discard([]) ->
  "";
top_of_discard([Head|_Tail]) ->
  Head#card.name.

ajax_response(GameName) ->
  Action = lists:nth(3, string:tokens(beepbeep_args:path(Env), "/")),
  case is_ajax_request() of
    true  -> {render, lists:concat(["game/", Action, ".html"]), []};
    false -> {redirect, lists:concat(["/game/", GameName])}
  end.

is_ajax_request() ->
  AllHeaders   = beepbeep_args:get_all_headers(Env),
  AcceptString = proplists:get_value("HTTP_ACCEPT", AllHeaders),
  AcceptList   = string:tokens(AcceptString, ", "),
  lists:member("application/json", AcceptList).

