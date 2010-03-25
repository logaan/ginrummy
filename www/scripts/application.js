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

  $("#value_sort").click(function() {
    jQuery.getJSON(game_path + "/value_sort");
    return false;
  });

  $("#knock").click(function() {
    jQuery.getJSON(game_path + "/knock");
    return false;
  });

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
  function apply_sort() {
    // $("#player_cards li").draggable();
    // $("#discard").droppable({
    //   drop: function(event, ui) {
    //     console.log("foo");
    //   }
    // });

    $("#player_cards").sortable({
      update: function(event, ui) {
        var card_names = $("#player_cards a").map(function(i, e){
          return $(e).text();
        });

        jQuery.post(
          game_path + "/manual_sort",
          {"card_names": JSON.stringify(card_names.toArray())},
          jQuery.noop,
          "json"
        );
      }
    });
  }
  apply_sort();

  // 
  // Longpoll comet
  // failed connections keeps track of how many times the comet request fails
  // each fail slows down the comet request by another 400ms and then tries
  // again. after 5 failures the comet polling stops.
  var failed_connections = 0;
  function comet_request() {
    jQuery.getJSON(game_path + "/comet", {}, function(data){
      if(data != null) {
        failed_connections = 0;
        update_page(data)
        apply_sort();
        comet_request();
      } else {
        failed_connections++;
        if ( failed_connections <= 5 ) {
          setTimeout(comet_request, 400 * failed_connections);
        }
      }
    });
  }

  comet_request();
});

function update_page(data) {
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
  display_opponent_hand(data.opponent_hand);

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

function display_opponent_hand(hand) {
  if( hand.length == 0 ) {
    return false;
  } else {
    var opponent_hand = document.createElement("ul");
    $(opponent_hand).attr("title", "Your opponent's hand");
    $(hand).map(function(index, card_name) {
      var list_element = document.createElement("li");
      $(list_element).text(card_name);
      $(opponent_hand).append(list_element);
    });

    $(opponent_hand).dialog({ modal: true });
  }
}

