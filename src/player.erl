-module(player).
-include("records.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([set_sort_preference/2, sort_hand/1]).

set_sort_preference(Player, NewSort) ->
  sort_hand(Player#player{sort = NewSort}).

sort_hand(Player = #player{sort=Sort, hand=Hand}) ->
  NewHand = sort_hand(Sort, Hand),
  Player#player{hand=NewHand}.

% Internal

sort_hand(none, Hand) -> Hand;
sort_hand(Sort, Hand) ->
  lists:sort(fun(#card{properties=Prop1}, #card{properties=Prop2}) ->
    proplists:get_value(Sort, Prop1) < proplists:get_value(Sort, Prop2)
  end, Hand).

% Tests

no_sort_test() ->
  Player  = example_player(),
  Player1 = sort_hand(Player),
  Player1 = Player.

suite_sort_test() ->
  Player  = example_player(suite),
  Player1 = sort_hand(Player),
  ?assertEqual(#player{name="foo", sort=suite, hand=[
      #card{name = "ace clubs",     properties = [{suite,"clubs"},   {value,1}]},
      #card{name = "five diamonds", properties = [{suite,"diamonds"},{value,5}]},
      #card{name = "seven hearts",  properties = [{suite,"hearts"},  {value,7}]},
      #card{name = "two spades",    properties = [{suite,"spades"},  {value,2}]}
    ]}, Player1).

value_sort_test() ->
  Player  = example_player(value),
  Player1 = sort_hand(Player),
  ?assertEqual(#player{name="foo", sort=value, hand=[
      #card{name = "ace clubs",     properties = [{suite,"clubs"},   {value,1}]},
      #card{name = "two spades",    properties = [{suite,"spades"},  {value,2}]},
      #card{name = "five diamonds", properties = [{suite,"diamonds"},{value,5}]},
      #card{name = "seven hearts",  properties = [{suite,"hearts"},  {value,7}]}
    ]}, Player1).

example_player() ->
  example_player(none).
example_player(Sort) ->
  #player{name="foo", sort=Sort, hand=[
    #card{name = "ace clubs",     properties = [{suite,"clubs"},   {value,1}]},
    #card{name = "two spades",    properties = [{suite,"spades"},  {value,2}]},
    #card{name = "five diamonds", properties = [{suite,"diamonds"},{value,5}]},
    #card{name = "seven hearts",  properties = [{suite,"hearts"},  {value,7}]}
  ]}.

