// Keeps RentLens's personal settings in this browser's localStorage, and
// shows the tip that ends the setup wizard.
// Storage can be blocked (private windows, strict privacy settings); the app
// then still works, it just asks again next time.
(function () {
  var KEY = "rentlens-settings";

  function read() {
    try { return window.localStorage.getItem(KEY); } catch (e) { return null; }
  }
  function write(value) {
    try {
      if (value === null) window.localStorage.removeItem(KEY);
      else window.localStorage.setItem(KEY, value);
    } catch (e) { /* storage unavailable: keep the settings for this session only */ }
  }

  // Point at an element with a one-off popover (after the setup wizard);
  // it closes on the next click anywhere
  function coach(m) {
    setTimeout(function () {
      var el = document.querySelector(m.selector);
      if (!el || !window.bootstrap) return;
      el.scrollIntoView({ behavior: "smooth", block: "center" });
      var pop = bootstrap.Popover.getOrCreateInstance(el, {
        title: m.title, content: m.text, trigger: "manual",
        placement: "bottom", customClass: "coach-popover"
      });
      pop.show();
      var hide = function () {
        pop.dispose();
        document.removeEventListener("click", hide, true);
      };
      setTimeout(function () { document.addEventListener("click", hide, true); }, 300);
    }, 600);   // let the Search page and its table draw first
  }

  // Shiny is loaded before this script, so the handlers can be registered
  // now. Every handler must take exactly one argument, or Shiny refuses it.
  Shiny.addCustomMessageHandler("rentlens-save-settings", function (json) { write(json); });
  Shiny.addCustomMessageHandler("rentlens-clear-settings", function (_) { write(null); });
  Shiny.addCustomMessageHandler("rentlens-coach", function (m) { coach(m); });

  $(document).on("shiny:connected", function () {
    // "none" tells the server there is nothing saved yet (first visit).
    // Deferred so it runs after Shiny has finished connecting: an error
    // thrown inside this event would stop Shiny from starting up.
    setTimeout(function () {
      Shiny.setInputValue("settings-stored", read() || "none");
    }, 0);
  });
})();
