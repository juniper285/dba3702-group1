// Keeps RentLens's personal settings in this browser's localStorage, shows
// the tip that ends the setup wizard, and suggests places from OneMap as
// the renter types (window.RentLens, used by the "Places that matter" box).
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

  // ---- Place search: suggestions from the OneMap Search API (no key needed;
  // it allows calls from any web page) ----
  function titleCase(s) {
    return s.toLowerCase().replace(/\b([a-z])/g, function (c) { return c.toUpperCase(); });
  }
  // Street address without the building name (which can repeat the place
  // or name a shop inside it), ending with the postal code; same as
  // place_address() in R
  function streetAddress(h) {
    var a = h.ADDRESS || "";
    if (h.BUILDING && h.BUILDING !== "NIL") a = a.replace(h.BUILDING, "");
    a = a.trim().replace(/SINGAPORE \d{6}$/, "").trim();
    if (!a) a = (h.ADDRESS || "").replace(/SINGAPORE \d{6}$/, "").trim();
    a = titleCase(a.replace(/\s+/g, " "));
    return /^\d{6}$/.test(h.POSTAL) ? a + ", Singapore " + h.POSTAL : a;
  }
  function searchPlaces(query, callback) {
    var self = this;
    if (!query || query.trim().length < 2) return callback();
    var url = "https://www.onemap.gov.sg/api/common/elastic/search?returnGeom=Y&getAddrDetails=Y&pageNum=1&searchVal=" +
      encodeURIComponent(query.trim());
    fetch(url)
      .then(function (r) { return r.json(); })
      .then(function (d) {
        var seen = {}, out = [];
        (d.results || []).forEach(function (h) {
          var key = h.SEARCHVAL + "|" + h.ADDRESS;
          if (seen[key] || out.length >= 8) return;
          seen[key] = true;
          var place = { name: h.SEARCHVAL, building: h.BUILDING, address: h.ADDRESS,
                        postal: h.POSTAL, lat: +h.LATITUDE, lng: +h.LONGITUDE };
          out.push({ value: JSON.stringify(place), label: titleCase(h.SEARCHVAL),
                     detail: streetAddress(h) });
        });
        // Show only this query's results, in OneMap's order
        if (self && self.clearOptions) self.clearOptions();
        callback(out);
      })
      .catch(function () { callback(); });
  }
  function renderPlace(item, escape) {
    return '<div class="place-option"><div class="place-option-name">' + escape(item.label) +
      '</div>' + (item.detail ? '<div class="place-option-detail">' + escape(item.detail) + '</div>' : '') +
      '</div>';
  }
  function renderTyped(data, escape) {
    return '<div class="create">Use \u201c' + escape(data.input) + '\u201d as typed</div>';
  }
  window.RentLens = { searchPlaces: searchPlaces, renderPlace: renderPlace, renderTyped: renderTyped };

  // ---- Optional settings (optional_block() in R): using a greyed-out
  // control turns its option on, "Clear" turns it off ----
  function activateOptional(e) {
    var body = e.target.closest && e.target.closest(".optional-body");
    if (!body) return;
    var sw = body.parentNode.querySelector(".optional-head input[type=checkbox]");
    if (sw && !sw.checked) { sw.checked = true; $(sw).trigger("change"); }
  }
  document.addEventListener("mousedown", activateOptional, true);
  document.addEventListener("focusin", activateOptional, true);
  $(document).on("click", ".optional-clear", function () {
    var sw = this.parentNode.querySelector("input[type=checkbox]");
    if (sw && sw.checked) { sw.checked = false; $(sw).trigger("change"); }
  });

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
