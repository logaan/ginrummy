% properties is an ordered_dict()
-record(card, {name, properties=[]}).
-record(game, {players=[], zones=[], chat_server, game_name}).
-record(player, {name, hand=[]}).

