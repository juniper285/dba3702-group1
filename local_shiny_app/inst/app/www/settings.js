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
  function coach(m, tries) {
    tries = tries || 0;
    setTimeout(function () {
      var el = document.querySelector(m.selector);
      // the table may still be drawing: try again for a few seconds
      if (!el && tries < 10) { coach(m, tries + 1); return; }
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
  // Title case that keeps acronyms ("NUS (Ridge View Residential College
  // (RVRC))"); same rule and word lists as title_case_place() in R
  var ACRONYMS = ["NUS", "NTU", "SMU", "SUTD", "SIT", "SUSS", "SIM", "NIE", "ITE", "NAFA",
                  "ACS", "SJI", "CHIJ", "SCGS", "MGS", "RI", "RGS", "HCI", "SOTA",
                  "NUH", "SGH", "CGH", "KTPH", "MOE", "HDB", "URA", "LTA", "MRT", "LRT",
                  "CBD", "MBS", "ECP", "PIE", "AYE", "CTE", "BKE", "KPE", "TPE", "SLE",
                  "UOB", "OCBC", "DBS", "POSB", "IMM", "JEM", "NEX", "ION", "YMCA", "YWCA"];
  var ABBREVS = ["ST", "RD", "DR", "PL", "CL", "CT", "LN", "GR", "SQ", "WK", "PK", "BT",
                 "TG", "MT", "STH", "NTH", "BLK", "BLVD", "JLN", "LOR", "LRG", "GDN",
                 "GDNS", "HTS", "CTR", "CRES", "TCE", "HWY", "WLK", "PKWY", "XPWY", "MKT"];
  var SMALL = ["of", "and", "the", "at", "in", "on", "by", "for", "to"];
  function titleCase(s) {
    var first = true;
    return s.replace(/[A-Za-z0-9]+/g, function (w, at) {
      var up = w.toUpperCase(), low = w.toLowerCase(), out;
      if (/[0-9]/.test(w) || ACRONYMS.indexOf(up) >= 0 ||
          (w.length >= 2 && w.length <= 5 && !/[AEIOUY]/.test(up) && ABBREVS.indexOf(up) < 0)) out = up;
      else if (at > 0 && s.charAt(at - 1) === "'") out = low;
      else if (!first && SMALL.indexOf(low) >= 0) out = low;
      else out = up.charAt(0) + low.slice(1);
      first = false;
      return out;
    });
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
  var ROAD = /^(JALAN|JLN|LORONG|LOR) |\b(ROAD|STREET|AVENUE|DRIVE|LANE|WAY|CRESCENT|CLOSE|LINK|RISE|WALK|TERRACE|BOULEVARD|EXPRESSWAY|HIGHWAY|RD|ST|AVE|DR)( \d+[A-Z]?)?$/;
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
          // A bare road (no postal code, named like a road) is pinned at its middle
          var road = !/^\d{6}$/.test(h.POSTAL) && h.ADDRESS === h.SEARCHVAL && ROAD.test(h.SEARCHVAL);
          out.push({ value: JSON.stringify(place), label: titleCase(h.SEARCHVAL),
                     detail: road ? "Whole street \u2013 add a block number for an exact spot"
                                  : streetAddress(h), road: road });
        });
        // Show only this query's results, in OneMap's order
        if (self && self.clearOptions) self.clearOptions();
        callback(out);
      })
      .catch(function () { callback(); });
  }
  function renderPlace(item, escape) {
    return '<div class="place-option"><div class="place-option-name">' + escape(item.label) +
      '</div>' + (item.detail ? '<div class="place-option-detail' + (item.road ? ' is-road' : '') + '">' +
      escape(item.detail) + '</div>' : '') +
      '</div>';
  }
  function renderTyped(data, escape) {
    return '<div class="create">Use \u201c' + escape(data.input) + '\u201d as typed</div>';
  }
  // ---- Floor area: slider labels in both units, plus typed limits ----
  // The slider's scale ends at its max (4,000 sq ft), which means "up to the
  // largest home" (data-top); the Lower/Upper limit boxes hold the exact
  // limits, in ft² and m², and any of the five can be changed.
  var SQFT_PER_SQM = 10.7639;
  function num(x, digits) { return (+x).toLocaleString("en-US", { maximumFractionDigits: digits || 0 }); }

  function areaParts(root) {
    var $root = $(root);
    return { root: $root, slider: $root.find("input.js-range-slider"),
             lo: $root.find(".area-limit.lo input.sqft"), loSqm: $root.find(".area-limit.lo input.sqm"),
             hi: $root.find(".area-limit.hi input.sqft"), hiSqm: $root.find(".area-limit.hi input.sqm"),
             top: +$root.data("top") };
  }
  // Label text; at the slider's end it shows the typed upper limit (or the
  // largest home), and the end label shows the largest home: CSS picks
  // .cur or .top, and hides the units on the 0 / max end labels
  function areaLabel(p, max) {
    return function (n) {
      var cur = n >= max ? (+p.hi.val() || p.top) : n;
      var top = n >= max ? '<span class="top">' + num(p.top) + "</span>" : "";
      return '<span class="val"><span class="cur">' + num(cur) + "</span>" + top +
        '<span class="unit"> ft²</span><span class="sqm">' + num(cur / SQFT_PER_SQM, 1) +
        " m²</span></span>";
    };
  }
  function setBox($box, v) {
    if (+$box.val() !== v) $box.val(v).trigger("change");   // "change" tells Shiny
  }
  var areaSyncing = false;
  function setupArea(el) {
    var p = areaParts(el.closest(".area-slider"));
    var slider = p.slider.data("ionRangeSlider");
    if (!slider) return;
    var max = +p.slider.data("max");
    slider.update({ prettify: areaLabel(p, max), force_edges: true, values_separator: " – " });
    if (p.root.data("area-ready")) return;
    p.root.data("area-ready", true);

    // slider -> boxes
    p.slider.on("change.rentlens", function () {
      if (areaSyncing) return;
      var r = slider.result;
      setBox(p.lo, r.from); p.loSqm.val((r.from / SQFT_PER_SQM).toFixed(1));
      // at the end, keep a typed limit above the slider's scale
      var hi = r.to >= max ? (+p.hi.val() >= max ? +p.hi.val() : p.top) : r.to;
      setBox(p.hi, hi); p.hiSqm.val((hi / SQFT_PER_SQM).toFixed(1));
    });
    // boxes -> slider (and the other unit)
    function fromBoxes() {
      var lo = Math.max(0, +p.lo.val() || 0), hi = +p.hi.val();
      if (!(hi > 0)) hi = p.top;
      areaSyncing = true;
      slider.update({ from: Math.min(lo, max), to: Math.min(Math.max(hi, lo), max) });
      areaSyncing = false;
    }
    p.lo.add(p.hi).on("input", function () {
      var $m = $(this).is(p.lo) ? p.loSqm : p.hiSqm;
      $m.val(this.value === "" ? "" : (+this.value / SQFT_PER_SQM).toFixed(1));
      fromBoxes();
    });
    p.loSqm.add(p.hiSqm).on("input", function () {
      var $f = $(this).is(p.loSqm) ? p.lo : p.hi;
      $f.val(this.value === "" ? "" : Math.round(+this.value * SQFT_PER_SQM)).trigger("change");
      fromBoxes();
    });
  }
  // Shiny sets its own labels when the slider is created or updated, so
  // swap them afterwards
  $(document).on("shiny:bound shiny:updateinput", ".area-slider input.js-range-slider", function (e) {
    var el = e.target;
    setTimeout(function () { setupArea(el); }, 0);
  });

  // ---- Tables (rl_datatable() in R): remember which columns each table
  // shows and how wide they are, by column name, so a new place's distance
  // column doesn't shift the others. Called on every render, so the choice
  // survives new filters.
  function tableStore(api) {
    var id = $(api.table().container()).closest(".datatables").attr("id");
    if (!id) return null;
    var key = "rentlens-columns-" + id;
    return {
      read: function () {
        try { return JSON.parse(window.localStorage.getItem(key) || "null") || {}; }
        catch (e) { return {}; }
      },
      write: function (s) {
        try { window.localStorage.setItem(key, JSON.stringify(s)); } catch (e) { /* this session only */ }
      }
    };
  }
  function colName(api, i) { return $(api.column(i).header()).text(); }

  // Give column i a fixed width (px), or null for its natural width. Body
  // cells hold the width; adjust() then copies it to the scrolling header.
  function setWidth(api, i, w) {
    var css = w ? w + "px" : "";
    $(api.column(i).header()).css({ width: css, minWidth: css });
    $(api.column(i).nodes()).css({ width: css, minWidth: css })
      .find(".dt-clip").css("max-width", w ? (w - 16) + "px" : "");
  }

  function setupTable(api) {
    var store = tableStore(api);
    if (!store) return;
    var saved = store.read();
    api.columns().every(function (i) {
      var n = colName(api, i);
      if ((saved.shown || []).indexOf(n) >= 0) this.visible(true, false);
      if ((saved.hidden || []).indexOf(n) >= 0) this.visible(false, false);
      if (saved.widths && saved.widths[n]) setWidth(api, i, saved.widths[n]);
    });
    api.columns.adjust();

    var save = function (change) {
      var s = store.read();
      change(s);
      store.write(s);
    };
    $(api.table().node()).off("column-visibility.dt.rentlens")
      .on("column-visibility.dt.rentlens", function () {
        save(function (s) {
          s.shown = []; s.hidden = [];
          api.columns().every(function (i) { (this.visible() ? s.shown : s.hidden).push(colName(api, i)); });
        });
      });

    // A drag handle on the right edge of each header (not the star column)
    api.columns().every(function (i) {
      var th = this.header();
      if (!th || th.querySelector(".dt-resize") || !colName(api, i).trim()) return;
      var grip = document.createElement("span");
      grip.className = "dt-resize";
      grip.title = "Drag to resize; double-click to reset";
      th.appendChild(grip);
      grip.addEventListener("click", function (e) { e.stopPropagation(); });   // don't sort
      grip.addEventListener("dblclick", function (e) {
        e.stopPropagation();
        setWidth(api, i, null);
        api.columns.adjust();
        save(function (s) { if (s.widths) delete s.widths[colName(api, i)]; });
      });
      grip.addEventListener("mousedown", function (e) {
        e.preventDefault(); e.stopPropagation();
        var x0 = e.clientX, w0 = th.getBoundingClientRect().width, w = w0, frame = null;
        document.body.classList.add("dt-resizing");
        var move = function (ev) {
          w = Math.max(48, Math.round(w0 + ev.clientX - x0));
          if (frame) return;
          frame = requestAnimationFrame(function () { frame = null; setWidth(api, i, w); api.columns.adjust(); });
        };
        var up = function () {
          document.removeEventListener("mousemove", move);
          document.removeEventListener("mouseup", up);
          document.body.classList.remove("dt-resizing");
          save(function (s) { s.widths = s.widths || {}; s.widths[colName(api, i)] = w; });
        };
        document.addEventListener("mousemove", move);
        document.addEventListener("mouseup", up);
      });
    });
  }

  // ---- The "My filters" box at the top of each page: a click anywhere on it
  // opens My filters, like its "Edit my filters" button (filter_box_ui())
  $(document).on("click", ".settings-summary", function (e) {
    if ($(e.target).closest(".edit-filters, a, button, input").length) return;
    var btn = this.querySelector(".edit-filters");
    if (btn) btn.click();
  });

  // ---- Stars (rl_datatable(star =) in R): one per row, adds the option to
  // the shortlist or takes it off. The server sends the starred options
  // ("rentlens-shortlist") whenever the shortlist changes.
  var starred = {};
  var STAR_SVG = '<svg viewBox="0 0 24 24" width="18" height="18" aria-hidden="true">' +
    '<path d="M12 3.2l2.7 5.6 6.1.8-4.5 4.2 1.1 6.1L12 17l-5.4 2.9 1.1-6.1-4.5-4.2 6.1-.8z"/></svg>';
  function starButton(key, inputId) {
    var on = !!starred[key];
    var safe = String(key).replace(/&/g, "&amp;").replace(/"/g, "&quot;");
    return '<button type="button" class="rl-star' + (on ? " is-on" : "") + '" data-key="' + safe +
      '" data-input="' + inputId + '" aria-pressed="' + on + '" title="' +
      (on ? "Remove from shortlist" : "Add to shortlist") + '">' + STAR_SVG + "</button>";
  }
  function paintStars() {
    $(".rl-star").each(function () {
      var on = !!starred[this.getAttribute("data-key")];
      this.classList.toggle("is-on", on);
      this.setAttribute("aria-pressed", on);
      this.title = on ? "Remove from shortlist" : "Add to shortlist";
    });
  }
  $(document).on("click", ".rl-star", function (e) {
    e.stopPropagation();   // a star click shouldn't also select the row
    this.classList.toggle("is-on");   // at once; the server confirms
    Shiny.setInputValue(this.getAttribute("data-input"),
                        { key: this.getAttribute("data-key"), at: Date.now() }, { priority: "event" });
  });

  window.RentLens = { searchPlaces: searchPlaces, renderPlace: renderPlace, renderTyped: renderTyped,
                      setupTable: setupTable, starButton: starButton };

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

  // The setup wizard shows each step as a new dialog. When one dialog
  // replaces another, Bootstrap can save the page's "no scrolling" style as
  // the one to restore, so the page couldn't scroll after the wizard. Once
  // the last dialog has closed, let the page scroll again.
  $(document).on("hidden.bs.modal", function () {
    if (document.querySelector(".modal.show")) return;
    document.body.style.removeProperty("overflow");
    document.body.style.removeProperty("padding-right");
    document.body.classList.remove("modal-open");
  });

  // Shiny is loaded before this script, so the handlers can be registered
  // now. Every handler must take exactly one argument, or Shiny refuses it.
  Shiny.addCustomMessageHandler("rentlens-save-settings", function (json) { write(json); });
  Shiny.addCustomMessageHandler("rentlens-clear-settings", function (_) { write(null); });
  Shiny.addCustomMessageHandler("rentlens-coach", function (m) { coach(m); });
  Shiny.addCustomMessageHandler("rentlens-shortlist", function (keys) {
    starred = {};
    (keys || []).forEach(function (k) { starred[k] = true; });
    paintStars();
  });
  // Saved map areas (mod_map.R), kept like the settings
  var AREAS_KEY = "rentlens-areas";
  Shiny.addCustomMessageHandler("rentlens-save-areas", function (json) {
    try { window.localStorage.setItem(AREAS_KEY, json); } catch (e) { /* this session only */ }
  });

  $(document).on("shiny:connected", function () {
    // "none" tells the server there is nothing saved yet (first visit).
    // Deferred so it runs after Shiny has finished connecting: an error
    // thrown inside this event would stop Shiny from starting up.
    setTimeout(function () {
      Shiny.setInputValue("settings-stored", read() || "none");
      var areas = null;
      try { areas = window.localStorage.getItem(AREAS_KEY); } catch (e) { /* blocked */ }
      Shiny.setInputValue("map-stored_areas", areas || "none");
    }, 0);
  });
})();
