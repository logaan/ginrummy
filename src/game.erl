-module(game).
-include_lib("stdlib/include/qlc.hrl").
-include("records.hrl").
-export([start_game/2, new_deck/0, library_draw/2, discard_draw/2, discard/3, test/0]).

start_game(Player1Name, Player2Name) ->
  Deck1 = new_deck(),
  {Player1Hand, Deck2} = move(10, [], Deck1),
  {Player2Hand, Deck3} = move(10, [], Deck2),
  {Discard,     Deck4} = move([], Deck3),
  Player1 = #player{ name = Player1Name, hand = Player1Hand },
  Player2 = #player{ name = Player2Name, hand = Player2Hand },
  {ok, Pid} = chat_server:start_link(),
  sort_hands(#game{ players=[Player1, Player2], deck=Deck4, discard=Discard, chat_server=Pid }).

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

library_draw(Number, Game = #game{players=Players, deck=Deck}) ->
  Player = lists:nth(Number, Players),
  {NewHand, NewDeck} = move(Player#player.hand, Deck),
  NewPlayer = Player#player{hand=NewHand},
  sort_hands(replace_player(Number, NewPlayer, Game#game{deck=NewDeck})).

discard_draw(Number, Game = #game{players=Players, discard=Discard}) ->
  Player = lists:nth(Number, Players),
  {NewHand, NewDiscard} = move(Player#player.hand, Discard),
  NewPlayer = Player#player{hand=NewHand},
  sort_hands(replace_player(Number, NewPlayer, Game#game{discard=NewDiscard})).

discard(Number, CardName, Game = #game{players=Players, discard=Discard}) ->
  Player = lists:nth(Number, Players),
  {value, Card, NewHand} = lists:keytake(CardName, 2, Player#player.hand),
  NewDiscard = [Card|Discard],
  NewPlayer = Player#player{hand=NewHand},
  sort_hands(replace_player(Number, NewPlayer, Game#game{discard=NewDiscard})).

move(ToDeck, [Card | FromDeck ]) ->
  {[Card | ToDeck], FromDeck}.
move(NumberOfCards, ToDeck, FromDeck) ->
  {Cards, NewFromDeck} = lists:split(NumberOfCards, FromDeck),
  NewToDeck = lists:append([Cards, ToDeck]),
  {NewToDeck, NewFromDeck}.

sort_hands(Game = #game{ players=[PlayerOne, PlayerTwo] }) ->
  CardCompare = fun(#card{properties=Prop1}, #card{properties=Prop2}) ->
    proplists:get_value(value, Prop1) > proplists:get_value(value, Prop2)
  end,
  Player1Hand = PlayerOne#player.hand,
  Player2Hand = PlayerTwo#player.hand,
  NewPlayer1Hand = lists:sort(CardCompare, Player1Hand),
  NewPlayer2Hand = lists:sort(CardCompare, Player2Hand),
  Game#game{
    players=[
      PlayerOne#player{ hand=NewPlayer1Hand },
      PlayerTwo#player{ hand=NewPlayer2Hand }
    ]
  }.
  
replace_player(Number, Player, Game = #game{ players=Players }) ->
  {Head, [_OldPlayer|Tail]} = lists:split(Number - 1, Players),
  NewPlayers = lists:append([Head, [Player], Tail]),
  Game#game{ players=NewPlayers }.

test() ->
  test_library_draw(),
  test_discard_draw(),
  test_discard(),
  ok.

test_library_draw() ->
  Game = start_game("foo", "bar"),
  #game{players=[P1|_], deck=Deck} = game:library_draw(1, Game),
  11 = length(P1#player.hand),
  30 = length(Deck).

test_discard_draw() ->
  Game = start_game("foo", "bar"),
  #game{players=[P1|_], discard=Discard} = game:discard_draw(1, Game),
  11 = length(P1#player.hand),
  0 = length(Discard).

test_discard() ->
  Game = #game{players=[OP1|_]} = start_game("foo", "bar"),
  [#card{name=CardName}|_] = OP1#player.hand,
  #game{players=[P1|_], discard=Discard} = game:discard(1, CardName, Game),
  9 = length(P1#player.hand),
  2 = length(Discard).
