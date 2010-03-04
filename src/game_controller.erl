-module(game_controller,[Env]).
-export([handle_request/2]).
-include("records.hrl").

handle_request("start",[]) ->
  PlayerOneName = beepbeep_args:get_param("player_one_name",Env),
  PlayerTwoName = beepbeep_args:get_param("player_two_name",Env),
  {GameName, _} = game_server:start(PlayerOneName, PlayerTwoName),
  beepbeep_args:set_session_data(GameName, player_one, Env),
  {redirect, lists:concat(["/game/", GameName])};

handle_request(GameName, []) ->
  AtomicGameName     = list_to_atom(GameName),
  {game_state, Game} = gen_server:call(AtomicGameName, game_state),
  PlayerOne          = Game#game.player1,
  PlayerTwo          = Game#game.player2,

  ViewData = case beepbeep_args:get_session_data(AtomicGameName, Env) of
    player_one -> view_data(Game, PlayerOne, PlayerTwo);
    player_two -> view_data(Game, PlayerTwo, PlayerOne);
    undefined ->
      beepbeep_args:set_session_data(GameName, player_one, Env),
      view_data(Game, PlayerTwo, PlayerOne)
  end,
  {render, "game/show.html", ViewData}.

view_data(Game, CurrentPlayer, Opponent) ->
  [
    {game,            io_lib:print(Game)},
    {player_one_name, CurrentPlayer#player.name},
    {player_two_name, Opponent#player.name},
    {card_count,      length(CurrentPlayer#player.hand)},
    {your_hand,       card_list(CurrentPlayer)},
    {top_of_discard,  top_of_discard(Game#game.discard)},
    {deck_size,       length(Game#game.deck)},
    {opponent_size,   length(Opponent#player.hand)}
  ].

card_list(Player) ->
  Hand = Player#player.hand,
  lists:map(fun(Card) -> Card#card.name end, Hand).

top_of_discard([]) ->
  false;
top_of_discard([Head|_Tail]) ->
  Head.

