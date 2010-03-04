-module(gin_rummy).
-include_lib("stdlib/include/qlc.hrl").
-include("records.hrl").
-export([start_game/2, new_deck/0, library_draw/2]).

start_game(Player1Name, Player2Name) ->
  Deck1 = new_deck(),
  {Player1Hand, Deck2} = move(10, [], Deck1),
  {Player2Hand, Deck3} = move(10, [], Deck2),
  Player1 = #player{ name = Player1Name, hand = Player1Hand },
  Player2 = #player{ name = Player2Name, hand = Player2Hand },
  #game{ player1 = Player1, player2 = Player2, deck = Deck3, discard = [] }.

new_deck() ->
  shuffle_deck(generate_playing_cards()).

generate_playing_cards() ->
  Suites = ["Hearts", "Diamonds", "Spades", "Clubs"],
  Values = ["King", "Queen", "Jack", "Ten", "Nine", "Eight", "Seven", "Six",
    "Five", "Four", "Three", "Two", "Ace"],
  [ #card{
      name = string:join([V, "of", S], " "),
      properties = [{suite, S}, {value, V}]
    } || S <- Suites, V <- Values ].

shuffle_deck(Deck) ->
  shuffle_deck([], Deck).
shuffle_deck(Deck, []) ->
  Deck;
shuffle_deck(Deck, OldDeck) ->
  {RandomCard, NewOldDeck} = random_draw(OldDeck),
  NewDeck = [RandomCard | Deck],
  shuffle_deck(NewDeck, NewOldDeck).

random_draw(Deck) ->
  Card = lists:nth(random:uniform(length(Deck)), Deck),
  NewDeck = lists:delete(Card, Deck),
  {Card, NewDeck}.

library_draw(player_one, Game = #game{player1=PlayerOne, deck=Deck}) ->
  {NewHand, NewDeck} = move(PlayerOne#player.hand, Deck),
  NewPlayerOne = PlayerOne#player{hand=NewHand},
  Game#game{player1=NewPlayerOne, deck=NewDeck};
library_draw(player_two, Game = #game{player2=PlayerTwo, deck=Deck}) ->
  {NewHand, NewDeck} = move(PlayerTwo#player.hand, Deck),
  NewPlayerTwo = PlayerTwo#player{hand=NewHand},
  Game#game{player2=NewPlayerTwo, deck=NewDeck}.

move(ToDeck, [Card | FromDeck ]) ->
  {[Card | ToDeck], FromDeck}.
move(NumberOfCards, ToDeck, FromDeck) ->
  {Cards, NewFromDeck} = lists:split(NumberOfCards, FromDeck),
  NewToDeck = lists:append([Cards, ToDeck]),
  {NewToDeck, NewFromDeck}.

