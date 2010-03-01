-module(gin_rummy).
-include_lib("stdlib/include/qlc.hrl").
-include("records.hrl").
-compile([export_all]).

setup_database() ->
  mnesia:delete_schema([node()]),
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(card, [{disc_copies, [node()]},
                              {attributes, record_info(fields, card)}]),
  mnesia:create_table(game, [{disc_copies, [node()]},
                              {attributes, record_info(fields, game)}]).

teardown_database() ->
  mnesia:stop(),
  mnesia:delete_schema([node()]).

insert_card(Card) ->
  Fun = fun() -> mnesia:write(Card) end,
  mnesia:transaction(Fun).

new_deck() ->
  Fun = fun() -> qlc:e(qlc:q([ C || C <- mnesia:table(card) ])) end,
  {atomic, Deck} = mnesia:transaction(Fun),
  shuffle_deck(Deck).

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

move(ToDeck, [Card | FromDeck ]) ->
  {[Card | ToDeck], FromDeck}.
move(NumberOfCards, ToDeck, FromDeck) ->
  {Cards, NewFromDeck} = lists:split(NumberOfCards, FromDeck),
  NewToDeck = lists:append([Cards, ToDeck]),
  {NewToDeck, NewFromDeck}.

generate_playing_cards() ->
  Suites = ["Hearts", "Diamonds", "Spades", "Clubs"],
  Values = ["King", "Queen", "Jack", "Ten", "Nine", "Eight", "Seven", "Six",
    "Five", "Four", "Three", "Two", "Ace"],
  [ #card{
      name = string:join([V, "of", S], " "),
      properties = [{suite, S}, {value, V}]
    } || S <- Suites, V <- Values ].

start_game(Player1Name, Player2Name) ->
  Deck1 = new_deck(),
  {Player1Hand, Deck2} = move(10, [], Deck1),
  {Player2Hand, Deck3} = move(10, [], Deck2),
  Player1 = #player{ name = Player1Name, hand = Player1Hand },
  Player2 = #player{ name = Player2Name, hand = Player2Hand },
  #game{ player1 = Player1, player2 = Player2, deck = Deck3, discard = [] }.

display_game(Game) ->
  io:format("~s~n", [io_lib:print(Game)]).

display_card({card, Name, _Properties}) ->
  io:format("~s~n", [Name]).

