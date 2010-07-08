jQuery(function() {
  var key_multiplier = 0;
  var game_path = window.location.pathname;

  //
  // Chat and Log
  $("#chat-field").focus();
  var chat_list = $("#scroller");
  chat_list.jScrollPane();
  if(chat_list[0].scrollTo != undefined) {
    chat_list[0].scrollTo(chat_list.attr("scrollHeight"));
  };

  $('#information-icon').click(function(){
    $('#information').toggle();
  });

  $('.colorbox').colorbox();

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

  $("#your-hand .face a").live("click", function() {
    var card_name = [$(this).find(".value").text(), $(this).find(".suit").text()].join(" ");
    jQuery.getJSON(game_path + "/discard/" + card_name);
    return false;
  });

  $("#discard a").click(discard_draw);

  $("#deck a").click(library_draw);

  $("#value_sort").click(function() {
    jQuery.getJSON(game_path + "/value_sort");
    return false;
  });

  $("#knock").click(function() {
    jQuery.getJSON(game_path + "/knock");
    return false;
  });

  $("#chat-container form").submit(function() {
    jQuery.post(
      game_path + "/broadcast",
      { message: $("#chat-field").val() },
      function() {  },
      "json"
    );
    $("#chat-field").val("").focus();
    return false;
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
      receive: function(event, ui) {
        console.log("received an element from ", ui.sender.attr("id"));
      },
      update: function(event, ui) {
        console.log("some card moved around from", event.originalTarget);
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
  $("#deck .remaining").text(data.deck_size);
  draw_opponent_hand(data.opponent_size);
  $("#player_size").text(data.player_size);
  $("#discard .remaining").text(data.top_of_discard);
  update_card_list(data.player_hand);
  add_new_messages(data.new_messages);
  display_opponent_hand(data.opponent_hand);

  return true;
};

// UPDATED FOR DESIGN
function update_card_list(cards) {
  console.log(cards);
  var game_path = window.location.pathname;

  $("#your-hand .cards").empty();

  $(cards).each(function(index, card_name){
    // pull the data from the card name
    var value_name = card_name.split(" ")[0];
    var suit_name  = card_name.split(" ")[1];

    // construct the card element
    var li = $("<li class='face'></li>").addClass(card_name);
    var link = $("<a></a>").attr("href", game_path + "/discard/" + card_name);
    var value = $("<span class='value'></span>").text(value_name);
    var suit = $("<span class='suit'></span>").text(suit_name);
    var card_element = li.append(link.append(value).append(suit));

    // inject it into the list
    $("#your-hand .cards").append(card_element);
  });
}

// UPDATED FOR DESIGN
// Inject new chat messages into the log and refresh the scroller
function add_new_messages(new_messages) {
  for(var i=0; i < new_messages.length; i++) {
    var list_element = document.createElement("li");
    $(list_element).text(new_messages[i]);
    $("#chat_messages").append(list_element);

    // Reload the scroller and point it at the bottom
    var chat_list = $("#scroller");
    chat_list.jScrollPane();
    if(chat_list[0].scrollTo != undefined) {
      chat_list[0].scrollTo(chat_list.attr("scrollHeight"));
    };
  };
}

// Pop up the knock dialog
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

    $(opponent_hand).dialog();
  }
}

// UPDATED FOR DESIGN
// Set the appropriate classes for the cards in the opponent's hand
function draw_opponent_hand(number_of_cards) {
  $("#their-hand .cards li").removeClass("back").removeClass("empty");
  $("#their-hand .cards li:lt(" + number_of_cards + ")").addClass("back");
  $("#their-hand .cards li:eq(" + parseInt(number_of_cards) + ")").addClass("empty");
  $("#their-hand .cards li:gt(" + parseInt(number_of_cards) + ")").addClass("empty");
};

