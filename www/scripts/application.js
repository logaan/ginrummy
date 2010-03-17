jQuery(function() {
  var key_multiplier = 0;
  var game_path = window.location.pathname;

  //
  // Click behaviors
  function discard_draw() {
    jQuery.getJSON(game_path + "/discard_draw");
    return false;
  }

  function library_draw() {
    jQuery.getJSON(game_path + "/library_draw");
    return false;
  }

  $(".card").live("click", function() {
    var card_name = $(this).text();
    jQuery.getJSON(game_path + "/discard/" + card_name);
    return false;
  });

  $("#discard").click(discard_draw);

  $("#library").click(library_draw);

  $("#chat form").submit(function() {
    jQuery.post(
      game_path + "/broadcast",
      { message: $("#message").val() },
      function() {  },
      "json"
    );
    $("#message").val("").focus();
    return false;
  });

  //
  // Keyboard behavior
  function multiplier_times_do(func) {
    func();
    for(var i = 1; i < key_multiplier; i++) {
      func();
    }
    key_multiplier = 0;
  }

  $(document).keypress(function(e) {
    if( ($(e.currentTarget.activeElement).attr("nodeName") != "INPUT") ) {
      switch( e.which ) {
        case 0: // escape
          key_multiplier = 0;
          e.preventDefault();
          break;

        case 100: // lower case d
          if( e.ctrlKey ) {
            multiplier_times_do(function() { discard_draw(); });
            e.preventDefault();
          }
          break;

        case 108: // lower case l
          if( e.ctrlKey ) {
            multiplier_times_do(function() { library_draw(); });
            e.preventDefault();
          }
          break;

        default: 
          if( e.which >= 48 && e.which <= 57 ) { // 0 to 9
            // Add the number onto the multiplier. So if you hit
            // 1 then 0 then 8 the multiplier would be 108.
            key_multiplier = key_multiplier * 10 + e.which - 48;
          };
      }
    }
  });

  //
  // Sorting
  $("#player_cards").sortable({
    update: function(event, ui) {
      var card_names = $("#player_cards a").map(function(i, e){
        return $(e).text();
      });

      jQuery.post(
        game_path + "/sort",
        {"card_names": JSON.stringify(card_names.toArray())},
        jQuery.noop,
        "json"
      );
    }
  });

  // 
  // Longpoll comet
  function comet_request() {
    jQuery.getJSON(game_path + "/comet", {}, function(data){
      update_page(data)
      comet_request();
    });
  }

  comet_request();
});

function update_page(data) {
  if(data == null) return false;

  $("#deck_size").text(data.deck_size);
  $("#opponent_size").text(data.opponent_size);
  $("#player_size").text(data.player_size);
  if (data.top_of_discard == "") {
    $("#discard").text("Empty")
  } else {
    $("#discard").text(data.top_of_discard);
  }
  update_card_list(data.player_hand);
  add_new_messages(data.new_messages);

  return true;
};

function update_card_list(cards) {
  var game_path = window.location.pathname;

  var card_list = document.createElement("ul");
  $(card_list).attr("id", "player_cards");

  for(var i = 0; i < cards.length; i++) {
    var card_name = cards[i];

    var card = document.createElement("a");
    $(card).addClass("card");
    $(card).attr("href", game_path + "/discard/" + card_name);
    $(card).text(card_name);

    var list_element = document.createElement("li");
    $(list_element).append(card);
    $(card_list).append(list_element);
  }

  $("#player_cards").replaceWith(card_list);
}

function add_new_messages(new_messages) {
  for(var i=0; i < new_messages.length; i++) {
    var list_element = document.createElement("li");
    $(list_element).text(new_messages[i]);
    $("#chat_messages").append(list_element);

    var chat_list = $("#chat ul");
    chat_list.attr("scrollTop", chat_list.attr("scrollHeight"));
  };
}

