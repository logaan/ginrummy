-module(game_controller,[Env]).

-export([handle_request/2]).

handle_request("start",[]) ->
    PlayerOne = beepbeep_args:get_param("player_one_name",Env),
    PlayerTwo = beepbeep_args:get_param("player_two_name",Env),

    {render,"game/start.html",[
      {player_one_name, PlayerOne},
      {player_two_name, PlayerTwo}
    ]}.

