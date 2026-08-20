/*
  Script for the fpdoc 'material' output backend.

  It renders the navigation tree from assets/nav.js, searches the identifier
  list in assets/search.js, switches between the light and dark colour scheme,
  copies code blocks and highlights the current entry in the page contents.
  Everything works from a local disk, no data is fetched from a server.
*/

(function () {
  "use strict";

  var body = document.body;
  var root = document.documentElement;
  var top = body.getAttribute("data-fpdoc-top") || "";
  var currentPage = body.getAttribute("data-fpdoc-page") || "";
  var currentUnit = body.getAttribute("data-fpdoc-unit") || "";

  function el(name, cls, text) {
    var e = document.createElement(name);
    if (cls) e.className = cls;
    if (text !== undefined && text !== null) e.appendChild(document.createTextNode(text));
    return e;
  }

  /* ------------------------------------------------------------------
     Colour scheme
     ------------------------------------------------------------------*/

  var SCHEME_KEY = "fpdoc-scheme";

  function schemeIcon(scheme) {
    if (scheme === "dark")
      return '<svg viewBox="0 0 24 24"><path d="M12 7a5 5 0 015 5 5 5 0 01-5 5 5 5 0 01-5-5 5 5 0 015-5m0-5l2.39 3.42C13.65 5.15 12.84 5 12 5c-.84 0-1.65.15-2.39.42L12 2M3.34 7l4.16-.35A7.2 7.2 0 005.94 8.5c-.44.74-.69 1.5-.83 2.29L3.34 7m.02 10l1.76-3.77a7.131 7.131 0 002.35 4.14L3.36 17M20.65 7l-1.77 3.79a7.023 7.023 0 00-2.34-4.15l4.11.36m-.01 10l-4.16.36c.58-.51 1.11-1.14 1.53-1.9.43-.74.69-1.5.83-2.29L20.64 17M12 22l-2.41-3.44c.74.27 1.55.44 2.41.44.82 0 1.63-.15 2.37-.42L12 22z"/></svg>';
    return '<svg viewBox="0 0 24 24"><path d="M17.75 4.09l-2.53 1.94.91 3.06-2.63-1.81-2.63 1.81.91-3.06-2.53-1.94L12.44 4l1.06-3 1.06 3 3.19.09m3.5 6.91l-1.64 1.25.59 1.98-1.7-1.17-1.7 1.17.59-1.98L15.75 11l2.06-.05.69-1.95.69 1.95 2.06.05m-2.28 4.95c.83-.08 1.72 1.1 1.19 1.85-.32.45-.66.87-1.08 1.27C15.17 23 8.84 23 4.94 19.07c-3.91-3.9-3.91-10.24 0-14.14.4-.4.82-.76 1.27-1.08.75-.53 1.93.36 1.85 1.19-.27 2.86.69 5.83 2.9 8.04 2.21 2.2 5.17 3.16 8.01 2.89m-1.64 2.02a12.08 12.08 0 01-7.79-3.47c-2.17-2.19-3.33-5-3.49-7.82-2.81 3.14-2.7 7.96.31 10.98 3.02 3.01 7.84 3.12 10.97.31z"/></svg>';
  }

  function currentScheme() {
    return root.getAttribute("data-fpdoc-scheme") || "light";
  }

  function applyScheme(scheme, store) {
    root.setAttribute("data-fpdoc-scheme", scheme);
    if (store) {
      try { localStorage.setItem(SCHEME_KEY, scheme); } catch (e) { /* private mode */ }
    }
    var button = document.getElementById("fpdoc-scheme");
    if (button) {
      var other = scheme === "dark" ? "light" : "dark";
      button.innerHTML = schemeIcon(scheme);
      button.title = "Switch to " + other + " mode";
      button.setAttribute("aria-label", button.title);
    }
  }

  function initScheme() {
    var button = document.getElementById("fpdoc-scheme");
    applyScheme(currentScheme(), false);
    if (!button) return;
    button.addEventListener("click", function () {
      applyScheme(currentScheme() === "dark" ? "light" : "dark", true);
    });
  }

  /* ------------------------------------------------------------------
     Navigation tree
     ------------------------------------------------------------------*/

  var CHEVRON = '<svg viewBox="0 0 24 24"><path d="M8.59 16.58L13.17 12 8.59 7.41 10 6l6 6-6 6-1.41-1.42z"/></svg>';

  function navLink(url, text, active) {
    var a = el("a", "md-nav__link" + (active ? " md-nav__link--active" : ""), text);
    a.href = top + url;
    return a;
  }

  function navSection(parent, title) {
    if (title) parent.appendChild(el("label", "md-nav__title", title));
    var list = el("ul", "md-nav__list");
    parent.appendChild(list);
    return list;
  }

  function renderNav(nav) {
    var navRoot = document.getElementById("fpdoc-nav");
    if (!navRoot) return;
    var i, item, list;

    navRoot.innerHTML = "";

    // Package level pages
    list = navSection(navRoot, nav.pkgTitle || nav.title);
    for (i = 0; i < nav.pkg.length; i++) {
      item = el("li", "md-nav__item");
      item.appendChild(navLink(nav.pkg[i][1], nav.pkg[i][0], nav.pkg[i][1] === currentPage));
      list.appendChild(item);
    }

    // Units, each with its own subpages
    list = navSection(navRoot, nav.unitsTitle);
    for (i = 0; i < nav.units.length; i++)
      list.appendChild(renderUnit(nav, nav.units[i]));

    showInSidebar(navRoot, navRoot.querySelector(".md-nav__link--active"));
  }

  // Scroll the sidebar, never the page, to bring the current page into view
  function showInSidebar(navRoot, active) {
    if (!active) return;
    var scroller = navRoot.parentNode;
    while (scroller && scroller !== document.body &&
           !(scroller.scrollHeight > scroller.clientHeight))
      scroller = scroller.parentNode;
    if (!scroller || scroller === document.body) return;
    var box = active.getBoundingClientRect();
    var view = scroller.getBoundingClientRect();
    if (box.top >= view.top && box.bottom <= view.bottom) return;
    scroller.scrollTop += (box.top - view.top) - (view.height - box.height) / 2;
  }

  function renderUnit(nav, unit) {
    var name = unit[0], descr = unit[1], pages = unit[2];
    // The page names the unit in lower case, the tree keeps the original spelling
    var isCurrent = name.toLowerCase() === currentUnit;
    var item = el("li", "md-nav__item md-nav__item--nested" + (isCurrent ? " md-nav__item--expanded" : ""));

    var button = el("button", "md-nav__link md-nav__link--nested", name);
    button.type = "button";
    button.setAttribute("aria-expanded", isCurrent ? "true" : "false");
    if (descr) button.title = descr;
    var icon = el("span", "md-nav__icon");
    icon.innerHTML = CHEVRON;
    button.appendChild(icon);
    button.addEventListener("click", function () {
      var expanded = item.className.indexOf("md-nav__item--expanded") >= 0;
      item.className = "md-nav__item md-nav__item--nested" + (expanded ? "" : " md-nav__item--expanded");
      button.setAttribute("aria-expanded", expanded ? "false" : "true");
    });
    item.appendChild(button);

    var sub = el("nav", "md-nav");
    var list = navSection(sub, null);
    for (var i = 0; i < pages.length; i++) {
      var page = pages[i];
      var entry = el("li", "md-nav__item");
      entry.appendChild(navLink(page[1], nav.labels[page[0]] || page[1], page[1] === currentPage));
      list.appendChild(entry);
    }
    item.appendChild(sub);
    return item;
  }

  /* ------------------------------------------------------------------
     Search
     ------------------------------------------------------------------*/

  var searchInput = document.getElementById("fpdoc-search");
  var searchOutput = document.getElementById("fpdoc-search-output");
  var searchList = document.getElementById("fpdoc-search-result");
  var searchMeta = document.getElementById("fpdoc-search-meta");
  var searchState = { loading: false, ready: false, names: null, selected: -1, query: "" };

  function searchLimit() {
    return parseInt(body.getAttribute("data-fpdoc-search-limit") || "30", 10);
  }

  function setSearchMeta(text) {
    if (searchMeta) searchMeta.textContent = text;
  }

  function openSearch(open) {
    if (!searchOutput) return;
    searchOutput.className = "md-search__output" + (open ? " md-search__output--open" : "");
  }

  function loadSearchIndex() {
    if (searchState.loading || searchState.ready) return;
    searchState.loading = true;
    setSearchMeta("Loading index...");
    var script = document.createElement("script");
    script.src = top + "assets/search.js";
    script.onload = function () {
      var index = window.fpdocIndex || [];
      searchState.names = new Array(index.length);
      for (var i = 0; i < index.length; i++)
        searchState.names[i] = index[i][0].toLowerCase();
      searchState.ready = true;
      searchState.loading = false;
      setSearchMeta(index.length + " identifiers");
      if (searchInput && searchInput.value) runSearch(searchInput.value);
    };
    script.onerror = function () {
      searchState.loading = false;
      setSearchMeta("Index not found");
    };
    document.body.appendChild(script);
  }

  function findMatches(query) {
    var index = window.fpdocIndex || [];
    var names = searchState.names;
    var limit = searchLimit();
    var exact = [], starts = [], member = [], contains = [];
    var q = query.toLowerCase();
    var total = 0;

    for (var i = 0; i < names.length; i++) {
      var name = names[i];
      var at = name.indexOf(q);
      if (at < 0) continue;
      total++;
      if (name === q) exact.push(i);
      else if (at === 0) starts.push(i);
      else if (name.charAt(at - 1) === ".") member.push(i);
      else contains.push(i);
      if (exact.length + starts.length + member.length > limit * 4) break;
    }

    function byName(a, b) { return names[a] < names[b] ? -1 : (names[a] > names[b] ? 1 : 0); }
    starts.sort(byName);
    member.sort(byName);
    contains.sort(byName);

    var all = exact.concat(starts, member, contains);
    var result = [];
    for (var j = 0; j < all.length && result.length < limit; j++)
      result.push(index[all[j]]);
    return { items: result, total: total };
  }

  function renderResults(found, query) {
    searchList.innerHTML = "";
    searchState.selected = -1;
    for (var i = 0; i < found.items.length; i++) {
      var entry = found.items[i];
      var item = el("li", "md-search-result__item");
      var link = el("a", "md-search-result__link");
      link.href = top + entry[3];
      link.appendChild(el("div", "md-search-result__title", entry[0]));
      var meta = el("div", "md-search-result__meta");
      meta.appendChild(el("span", "md-search-result__kind", entry[2]));
      meta.appendChild(document.createTextNode(entry[1]));
      link.appendChild(meta);
      if (entry[4]) link.appendChild(el("div", "md-search-result__teaser", entry[4]));
      item.appendChild(link);
      searchList.appendChild(item);
    }
    if (found.total === 0)
      setSearchMeta("No match for '" + query + "'");
    else if (found.total > found.items.length)
      setSearchMeta(found.items.length + " of " + found.total + " matches");
    else
      setSearchMeta(found.total + (found.total === 1 ? " match" : " matches"));
  }

  function runSearch(query) {
    query = query.replace(/^\s+|\s+$/g, "");
    searchState.query = query;
    if (!searchState.ready) {
      loadSearchIndex();
      openSearch(query.length > 0);
      return;
    }
    if (query.length === 0) {
      searchList.innerHTML = "";
      setSearchMeta((window.fpdocIndex || []).length + " identifiers");
      openSearch(false);
      return;
    }
    renderResults(findMatches(query), query);
    openSearch(true);
  }

  function moveSelection(step) {
    var items = searchList.getElementsByClassName("md-search-result__link");
    if (items.length === 0) return;
    if (searchState.selected >= 0 && searchState.selected < items.length)
      items[searchState.selected].className = "md-search-result__link";
    searchState.selected += step;
    if (searchState.selected < 0) searchState.selected = items.length - 1;
    if (searchState.selected >= items.length) searchState.selected = 0;
    var selected = items[searchState.selected];
    selected.className = "md-search-result__link md-search-result__link--active";
    selected.scrollIntoView({ block: "nearest" });
  }

  function initSearch() {
    if (!searchInput || !searchList) return;

    searchInput.addEventListener("focus", loadSearchIndex);
    searchInput.addEventListener("input", function () { runSearch(searchInput.value); });

    searchInput.addEventListener("keydown", function (event) {
      switch (event.key) {
        case "ArrowDown": moveSelection(1); event.preventDefault(); break;
        case "ArrowUp": moveSelection(-1); event.preventDefault(); break;
        case "Enter":
          var items = searchList.getElementsByClassName("md-search-result__link");
          if (searchState.selected >= 0 && items[searchState.selected]) {
            window.location.href = items[searchState.selected].href;
            event.preventDefault();
          } else if (items.length === 1) {
            window.location.href = items[0].href;
            event.preventDefault();
          }
          break;
        case "Escape": openSearch(false); searchInput.blur(); break;
      }
    });

    document.addEventListener("click", function (event) {
      var node = event.target;
      while (node && node !== document.body) {
        if (node.className === "md-search") return;
        node = node.parentNode;
      }
      openSearch(false);
    });

    document.addEventListener("keydown", function (event) {
      if (event.key !== "/" || event.ctrlKey || event.altKey || event.metaKey) return;
      var tag = (event.target.tagName || "").toLowerCase();
      if (tag === "input" || tag === "textarea") return;
      searchInput.focus();
      event.preventDefault();
    });
  }

  /* ------------------------------------------------------------------
     Copy buttons on code blocks
     ------------------------------------------------------------------*/

  var COPY_ICON = '<svg viewBox="0 0 24 24"><path d="M19 21H8V7h11m0-2H8a2 2 0 00-2 2v14a2 2 0 002 2h11a2 2 0 002-2V7a2 2 0 00-2-2m-3-4H4a2 2 0 00-2 2v14h2V3h12V1z"/></svg>';
  var DONE_ICON = '<svg viewBox="0 0 24 24"><path d="M21 7L9 19l-5.5-5.5 1.41-1.41L9 16.17 19.59 5.59 21 7z"/></svg>';

  function copyCode(button, block) {
    var text = block.textContent;
    function done(ok) {
      button.innerHTML = ok ? DONE_ICON : COPY_ICON;
      button.className = "md-clipboard" + (ok ? " md-clipboard--copied" : "");
      window.setTimeout(function () {
        button.innerHTML = COPY_ICON;
        button.className = "md-clipboard";
      }, 1500);
    }
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(text).then(function () { done(true); }, function () { done(false); });
      return;
    }
    var area = document.createElement("textarea");
    area.value = text;
    area.style.position = "fixed";
    area.style.left = "-1000px";
    document.body.appendChild(area);
    area.select();
    var ok = false;
    try { ok = document.execCommand("copy"); } catch (e) { ok = false; }
    document.body.removeChild(area);
    done(ok);
  }

  function initClipboard() {
    var buttons = document.getElementsByClassName("md-clipboard");
    for (var i = 0; i < buttons.length; i++) {
      (function (button) {
        button.innerHTML = COPY_ICON;
        button.addEventListener("click", function () {
          var block = button.parentNode.getElementsByTagName("code")[0] ||
                      button.parentNode.getElementsByTagName("pre")[0];
          if (block) copyCode(button, block);
        });
      })(buttons[i]);
    }
  }

  /* ------------------------------------------------------------------
     Page contents: mark the heading that is being read
     ------------------------------------------------------------------*/

  function initTOC() {
    var toc = document.getElementById("fpdoc-toc");
    if (!toc) return;
    var links = toc.getElementsByClassName("md-nav__link");
    var targets = [];
    for (var i = 0; i < links.length; i++) {
      var id = links[i].getAttribute("href").substring(1);
      var target = document.getElementById(id);
      if (target) targets.push({ link: links[i], target: target });
    }
    if (targets.length === 0) return;

    var scheduled = false;

    function update() {
      scheduled = false;
      var current = -1;
      for (var i = 0; i < targets.length; i++)
        if (targets[i].target.getBoundingClientRect().top < 100) current = i;
      for (var j = 0; j < targets.length; j++)
        targets[j].link.className = "md-nav__link" + (j === current ? " md-nav__link--active" : "");
    }

    window.addEventListener("scroll", function () {
      if (scheduled) return;
      scheduled = true;
      window.requestAnimationFrame(update);
    }, { passive: true });
    update();
  }

  /* ------------------------------------------------------------------
     Back to top
     ------------------------------------------------------------------*/

  function initBackToTop() {
    var button = document.getElementById("fpdoc-top");
    if (!button) return;
    button.addEventListener("click", function () {
      window.scrollTo({ top: 0, behavior: "smooth" });
    });
    var scheduled = false;
    window.addEventListener("scroll", function () {
      if (scheduled) return;
      scheduled = true;
      window.requestAnimationFrame(function () {
        scheduled = false;
        button.className = "md-top" + (window.pageYOffset > 400 ? " md-top--show" : "");
      });
    }, { passive: true });
  }

  /* ------------------------------------------------------------------
     Start
     ------------------------------------------------------------------*/

  function start() {
    document.documentElement.className =
      document.documentElement.className.replace(/\bno-js\b/, "js");
    initScheme();
    if (window.fpdocNav) renderNav(window.fpdocNav);
    initSearch();
    initClipboard();
    initTOC();
    initBackToTop();
  }

  if (document.readyState === "loading")
    document.addEventListener("DOMContentLoaded", start);
  else
    start();
})();
