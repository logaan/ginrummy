-module(game).
-include_lib("stdlib/include/qlc.hrl").
-include("records.hrl").
-export([players/1, get_player/2, zones/1, chat_server/1]).
-export([start_game/3, restart_game/1, new_deck/0, library_draw/2,
         discard_draw/2, discard/3, manual_sort/3, value_sort/2, test/0]).

% Accessors
players(#game{ players=Players }) -> Players.
zones(#game{ zones=Zones }) -> Zones.
chat_server(#game{ chat_server=ChatServer }) -> ChatServer.

% Real API
start_game(Player1Name, Player2Name, GameName) ->
  {Player1Hand, Deck1} = move(10, [], new_deck()),
  {Player2Hand, Deck2} = move(10, [], Deck1),
  {Discard, Deck3} = move([], Deck2),

  Zones = [{discard, Discard}, {deck, Deck3}],
  Player1 = #player{ name = Player1Name, hand = Player1Hand },
  Player2 = #player{ name = Player2Name, hand = Player2Hand },
  {ok, ChatServer} = chat_server:start_link(),
  #game{ players=[Player1, Player2], zones=Zones, chat_server=ChatServer, game_name=GameName }.

restart_game(#game{ players=[Player1, Player2], game_name=GameName}) ->
  #player{ name = Player1Name } = Player1,
  #player{ name = Player2Name } = Player2,
  start_game(Player1Name, Player2Name, GameName).

new_deck() ->
  shuffle_deck(generate_playing_cards()).

generate_playing_cards() ->
  Suites = ["hearts", "diamonds", "spades", "clubs"],
  Values = [
    {"king",  13},
    {"queen", 12},
    {"jack",  11},
    {"ten",   10},
    {"nine",  9},
    {"eight", 8},
    {"seven", 7},
    {"six",   6},
    {"five",  5},
    {"four",  4},
    {"three", 3},
    {"two",   2},
    {"ace",   1}
  ],
  [ #card{
      name = string:join([ValName, Suite], " "),
      properties = [{suite, Suite}, {value, Val}]
    } || Suite <- Suites, {ValName, Val} <- Values ].

shuffle_deck(Deck) ->
  shuffle_deck([], Deck).
shuffle_deck(Deck, []) ->
  Deck;
shuffle_deck(Deck, OldDeck) ->
  {RandomCard, NewOldDeck} = random_draw(OldDeck),
  NewDeck = [RandomCard | Deck],
  shuffle_deck(NewDeck, NewOldDeck).

random_draw(Deck) ->
  Card = lists:nth(crypto:rand_uniform(1, length(Deck) + 1), Deck),
  NewDeck = lists:delete(Card, Deck),
  {Card, NewDeck}.

library_draw(Number, Game = #game{players=Players}) ->
  Player = lists:nth(Number, Players),
  Hand = Player#player.hand,
  case length(Hand) >= 11 of
    true -> {error, "Your hand is full"};
    false -> hand_move(deck, Number, fun move/2, Game)
  end.

discard_draw(Number, Game = #game{players=Players}) ->
  Player = lists:nth(Number, Players),
  Hand = Player#player.hand,
  case length(Hand) >= 11 of
    true -> {error, "Your hand is full"};
    false -> hand_move(discard, Number, fun move/2, Game)
  end.

discard(Number, CardName, Game) ->
  Strategy = fun(Hand, Discard) ->
    {value, Card, NewHand} = lists:keytake(CardName, 2, Hand),
    NewDiscard = [Card|Discard],
    {NewHand, NewDiscard}
  end,
  hand_move(discard, Number, Strategy, Game).

hand_move(ZoneName, PlayerNumber, Strategy, Game = #game{players=Players, zones=Zones}) ->
  % Decompose
  Player = lists:nth(PlayerNumber, Players),
  Zone   = proplists:get_value(ZoneName, Zones),
  % Do stuff
  {NewHand, NewZone} = Strategy(Player#player.hand, Zone),
  % Recompose
  NewPlayer = Player#player{hand=NewHand},
  NewGame   = Game#game{zones=replace_property({ZoneName, NewZone}, Zones)},
  replace_player(PlayerNumber, NewPlayer, NewGame).

move(ToDeck, [Card | FromDeck ]) ->
  {[Card | ToDeck], FromDeck};
move(ToDeck, FromDeck = []) ->
  {ToDeck, FromDeck}.
move(NumberOfCards, ToDeck, FromDeck) ->
  {Cards, NewFromDeck} = lists:split(NumberOfCards, FromDeck),
  NewToDeck = lists:append([Cards, ToDeck]),
  {NewToDeck, NewFromDeck}.

manual_sort(PlayerNumber, NewOrder, Game) ->
  Player = get_player(PlayerNumber, Game),
  Hand = Player#player.hand,
  OrderedIntersection = [RealCard || OrderCard <- NewOrder, RealCard = #card{name=Name} <- Hand, OrderCard == Name ],
  NotInOrder = fun(#card{name=Name}) -> not lists:member(Name, NewOrder) end,
  NewCards = lists:filter(NotInOrder, Hand),
  NewHand = lists:append(OrderedIntersection, NewCards),
  replace_player(PlayerNumber, Player#player{hand=NewHand}, Game).

get_player(Number, #game{players=Players}) ->
  lists:nth(Number, Players).

value_sort(PlayerNumber, Game) ->
  Player = get_player(PlayerNumber, Game),
  Hand = Player#player.hand,
  CardCompare = fun(#card{properties=Prop1}, #card{properties=Prop2}) ->
    proplists:get_value(value, Prop1) > proplists:get_value(value, Prop2)
  end,
  NewHand = lists:sort(CardCompare, Hand),
  replace_player(PlayerNumber, Player#player{ hand=NewHand }, Game).
  
replace_player(Number, Player, Game = #game{ players=Players }) ->
  {Head, [_OldPlayer|Tail]} = lists:split(Number - 1, Players),
  NewPlayers = lists:append([Head, [Player], Tail]),
  Game#game{ players=NewPlayers }.

replace_property(NewProp={Key, _Value}, Proplist) ->
  [NewProp | proplists:delete(Key, Proplist)].

test() ->
  Game1 = start_game("foo", "bar", foo),

  Game2 = #game{players=[P11|_], zones=Zones1} = game:discard_draw(1, Game1),
  Discard1 = proplists:get_value(discard, Zones1),
  0 = length(P11#player.hand),
  0 = length(Discard1),

  Game3 = #game{players=[P12|_]} = game:library_draw(1, Game2),
  [#card{name=CardName}|_] = P12#player.hand,

  #game{players=[P13|_], zones=Zones2} = game:discard(1, CardName, Game3),
  Discard2 = proplists:get_value(discard, Zones2),
  0 = length(P13#player.hand),
  1 = length(Discard2),

  ok.

