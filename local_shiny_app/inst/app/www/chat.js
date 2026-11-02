// "Ask RentLens" pop-up (mod_chat.R): drag the window by its title bar.
// The position is kept in this browser so it reopens where it was left.
(function () {
  var POS_KEY = "rentlens-chat-pos";

  function load() {
    try { return JSON.parse(localStorage.getItem(POS_KEY)) || { x: 0, y: 0 }; }
    catch (e) { return { x: 0, y: 0 }; }
  }
  function save(pos) {
    try { localStorage.setItem(POS_KEY, JSON.stringify(pos)); } catch (e) {}
  }

  // Keep the title bar on screen; x and y move the window from its
  // bottom-right starting place
  function clamp(dialog, pos) {
    dialog.style.transform = "";
    var r = dialog.getBoundingClientRect();
    var x = Math.min(Math.max(pos.x, -r.left), window.innerWidth - r.right);
    var y = Math.min(Math.max(pos.y, -r.top), window.innerHeight - r.top - 48);
    return { x: x, y: y };
  }
  function place(dialog, pos) {
    pos = clamp(dialog, pos);
    dialog.style.transform = "translate(" + pos.x + "px, " + pos.y + "px)";
    return pos;
  }

  document.addEventListener("DOMContentLoaded", function () {
    var win = document.querySelector(".chat-window");
    if (!win) return;
    var dialog = win.querySelector(".chat-dialog");
    var handle = win.querySelector(".modal-header");
    var pos = load();

    win.addEventListener("shown.bs.modal", function () {
      pos = place(dialog, pos);
      var input = win.querySelector("textarea");
      if (input) input.focus();
    });
    window.addEventListener("resize", function () { if (win.classList.contains("show")) pos = place(dialog, pos); });

    handle.addEventListener("pointerdown", function (e) {
      if (e.target.closest(".btn-close")) return;
      var start = { x: e.clientX, y: e.clientY, pos: pos };
      handle.setPointerCapture(e.pointerId);
      function move(ev) {
        pos = place(dialog, { x: start.pos.x + ev.clientX - start.x,
                              y: start.pos.y + ev.clientY - start.y });
      }
      function up() {
        handle.removeEventListener("pointermove", move);
        handle.removeEventListener("pointerup", up);
        save(pos);
      }
      handle.addEventListener("pointermove", move);
      handle.addEventListener("pointerup", up);
    });
    // Click the darkened page around the window to close it
    win.addEventListener("click", function (e) {
      if (e.target === win) bootstrap.Modal.getOrCreateInstance(win).hide();
    });
    // Double-click the title bar to put the window back in its corner
    handle.addEventListener("dblclick", function () { pos = place(dialog, { x: 0, y: 0 }); save(pos); });
  });
})();
