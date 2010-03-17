-module(game).
-include_lib("stdlib/include/qlc.hrl").
-include("records.hrl").
-export([start_game/2, new_deck/0, library_draw/2, discard_draw/2, discard/3, sort_hand/3, test/0]).

start_game(Player1Name, Player2Name) ->
  Zones = [{discard, []}, {deck, new_deck()}],
  Player1 = #player{ name = Player1Name },
  Player2 = #player{ name = Player2Name },
  {ok, ChatServer} = chat_server:start_link(),
  sort_hands(#game{ players=[Player1, Player2], zones=Zones, chat_server=ChatServer }).

new_deck() ->
  shuffle_deck(generate_playing_cards()).

generate_playing_cards() ->
  Suites = ["Hearts", "Diamonds", "Spades", "Clubs"],
  Values = [
    {"King",  13},
    {"Queen", 12},
    {"Jack",  11},
    {"Ten",   10},
    {"Nine",  9},
    {"Eight", 8},
    {"Seven", 7},
    {"Six",   6},
    {"Five",  5},
    {"Four",  4},
    {"Three", 3},
    {"Two",   2},
    {"Ace",   1}
  ],
  [ #card{
      name = string:join([ValName, "of", S], " "),
      properties = [{suite, S}, {value, Val}]
    } || S <- Suites, {ValName, Val} <- Values ].

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

library_draw(Number, Game) ->
  hand_move(deck, Number, fun move/2, Game).

discard_draw(Number, Game) ->
  hand_move(discard, Number, fun move/2, Game).

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
  sort_hands(replace_player(PlayerNumber, NewPlayer, NewGame)).

move(ToDeck, [Card | FromDeck ]) ->
  {[Card | ToDeck], FromDeck};
move(ToDeck, FromDeck = []) ->
  {ToDeck, FromDeck}.

sort_hand(PlayerNumber, NewOrder, Game) ->
  Player = get_player(PlayerNumber, Game),
  Hand = Player#player.hand,
  OrderedIntersection = [RealCard || OrderCard <- NewOrder, RealCard = #card{name=Name} <- Hand, OrderCard == Name ],
  NewCards = [Card || Card = #card{name=Name} <- Hand, not lists:member(Name, NewOrder)],
  io:format("NewCards: ~p~n", [NewCards]),
  NewHand = lists:append(OrderedIntersection, NewCards),
  replace_player(PlayerNumber, Player#player{hand=NewHand}, Game).

get_player(Number, #game{players=Players}) ->
  lists:nth(Number, Players).

sort_hands(Game = #game{ players=[PlayerOne, PlayerTwo] }) ->
  Game.
  
replace_player(Number, Player, Game = #game{ players=Players }) ->
  {Head, [_OldPlayer|Tail]} = lists:split(Number - 1, Players),
  NewPlayers = lists:append([Head, [Player], Tail]),
  Game#game{ players=NewPlayers }.

replace_property(NewProp={Key, _Value}, Proplist) ->
  [NewProp | proplists:delete(Key, Proplist)].

test() ->
  Game1 = start_game("foo", "bar"),

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

