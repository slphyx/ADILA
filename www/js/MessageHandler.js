Shiny.addCustomMessageHandler('toggleClass', function(data) {
  var el = document.getElementById(data.id);
  if (el) {
    if (data.enable) {
      el.classList.add(data.class);
    } else {
      el.classList.remove(data.class);
    }
  }
});
