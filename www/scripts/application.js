jQuery(function() {
  var game_path = window.location.pathname;

  $(".card").live("click", function() {
    var card_name = $(this).text();
    jQuery.getJSON(game_path + "/discard/" + card_name);
    return false;
  });

  $("#discard").click(function() {
    jQuery.getJSON(game_path + "/discard_draw");
    return false;
  });

  $("#library").click(function() {
    jQuery.getJSON(game_path + "/library_draw");
    return false;
  });

  $("#chat form").submit(function() {
    jQuery.post(
      game_path + "/broadcast",
      { message: $("#message").val() },
      function() { console.log("posted") },
      "json"
    );
    $("#message").val("").focus();
    return false;
  });

  $("#message").focus();

  function comet_request() {
    jQuery.getJSON(game_path + "/comet", {}, function(data){
      update_page(data);
      comet_request();
    });
  }

  comet_request();
});

function update_page(data) {
  // basic data swapping
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

