% properties is an ordered_dict()
-record(card, {name, properties=[]}).
-record(game, {players=[], zones=[], chat_server}).
-record(player, {name, hand=[]}).

