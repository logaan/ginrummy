-module(player).
-include("records.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([set_sort_preference/2, sort_hand/1]).
-import(proplists, [get_value/2]).

set_sort_preference(Player, NewSort) ->
  sort_hand(Player#player{sort = NewSort}).

sort_hand(Player = #player{sort=Sort, hand=Hand}) ->
  NewHand = sort_hand(Sort, Hand),
  Player#player{hand=NewHand}.

% Internal
sort_hand(none, Hand) -> Hand;
sort_hand(Sort, Hand) ->
  lists:sort(fun(#card{properties=Prop1}, #card{properties=Prop2}) ->
    case get_value(Sort, Prop1) == get_value(Sort, Prop2) of
      true -> get_value(other_sort(Sort), Prop1) < get_value(other_sort(Sort), Prop2);
      false -> get_value(Sort, Prop1) < get_value(Sort, Prop2)
    end
  end, Hand).

other_sort(value) -> suit;
other_sort(suit) -> value.

% Tests
no_sort_test() ->
  Player  = example_player(),
  Player1 = sort_hand(Player),
  Player1 = Player.

suit_sort_test() ->
  Player  = example_player(suit),
  Player1 = sort_hand(Player),
  ?assertEqual(#player{name="foo", sort=suit, hand=[
      #card{name = "ace clubs",     properties = [{suit,"clubs"},   {value,1}]},
      #card{name = "five diamonds", properties = [{suit,"diamonds"},{value,5}]},
      #card{name = "seven hearts",  properties = [{suit,"hearts"},  {value,7}]},
      #card{name = "two spades",    properties = [{suit,"spades"},  {value,2}]},
      #card{name = "five spades",   properties = [{suit,"spades"},  {value,5}]}
    ]}, Player1).

value_sort_test() ->
  Player  = example_player(value),
  Player1 = sort_hand(Player),
  ?assertEqual(#player{name="foo", sort=value, hand=[
      #card{name = "ace clubs",     properties = [{suit,"clubs"},   {value,1}]},
      #card{name = "two spades",    properties = [{suit,"spades"},  {value,2}]},
      #card{name = "five diamonds", properties = [{suit,"diamonds"},{value,5}]},
      #card{name = "five spades",   properties = [{suit,"spades"},  {value,5}]},
      #card{name = "seven hearts",  properties = [{suit,"hearts"},  {value,7}]}
    ]}, Player1).

example_player() ->
  example_player(none).
example_player(Sort) ->
  #player{name="foo", sort=Sort, hand=[
    #card{name = "five spades",   properties = [{suit,"spades"},  {value,5}]},
    #card{name = "ace clubs",     properties = [{suit,"clubs"},   {value,1}]},
    #card{name = "two spades",    properties = [{suit,"spades"},  {value,2}]},
    #card{name = "five diamonds", properties = [{suit,"diamonds"},{value,5}]},
    #card{name = "seven hearts",  properties = [{suit,"hearts"},  {value,7}]}
  ]}.

