function share_this_game() {
  var share = {
   method: 'stream.share',
   u: window.location.href
  };

  FB.ui(share, function(response) {
    console.log("Share responded:", response);
  });
}

function login_and_share() {
  FB.login(function(response) {
    if (response.session) {
      if (response.perms) {
        share_this_game();
      } else {
        alert("You need to grant permissions to be able to share this game");
      }
    } else {
      alert("You must be logged into facebook to share this game");
    }
  }, {perms:'publish_stream'});
}

$("body").append("<div id='fb-root'></div>");
$("#header").append("<span><a id='fb-share' href='#'>SHARE</a> this on facebook</span>");
$("#fb-share').click(login_and_share);

window.fbAsyncInit = function() {
  FB.init({
    appId  : '163918146967939',
    status : true, // check login status
    cookie : true, // enable cookies to allow the server to access the session
    xfbml  : true  // parse XFBML
  });
};

(function() {
  var e = document.createElement('script');
  e.src = document.location.protocol + '//connect.facebook.net/en_US/all.js';
  e.async = true;
  document.getElementById('fb-root').appendChild(e);
}());

