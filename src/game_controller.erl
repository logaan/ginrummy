-module(game_controller,[Env]).

-export([handle_request/2]).

handle_request("start",[]) ->
    PlayerOne = beepbeep_args:get_param("player_one_name",Env),
    PlayerTwo = beepbeep_args:get_param("player_two_name",Env),

    Game = gin_rummy:start_game(PlayerOne, PlayerTwo),

    {render,"game/start.html",[
      {game, io_lib:print(Game)},
      {player_one_name, PlayerOne},
      {player_two_name, PlayerTwo}
    ]}.

