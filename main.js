'use strict';

/*
TODO:
- Proof.render is a fucking disaster and NEEDS to be improved
  - Then, change show.js::asText
*/

/*
POSSIBLE FEATURES:
- Highlight selected line
- Highlight direct dependencies of selected line
  (i.e. the lines referenced in its justification)
- Highlight lines that depend on an invalid line,
  or lines that depend on one of these lines
*/

// requires: parse.js, serialize.js, justify.js

let $root = $('#proof-root');

let $root_ = $root;  // To be able to use it as a default value of a parameter
function getLocation(item, $root) {
  $root = $root || $($root_.children()[0]);  // Get top-level context
  /* Return the location of a item, or null if it doesn't exist.
     Note that `item` should be a DOM element but NOT a jQuery object */
  // (DFS)
  let children = $root.children();
  for (let i = 0; i < children.length; i++) {
    let child = children[i];
    if (child === item) {
      return [i];
    }

    let $child = $(child);
    if (!$child.hasClass("line") && !$child.hasClass("context")) {
      continue;
    }

    let rec = getLocation(item, $child);
    if (rec !== undefined) {
      return [i].concat(rec);
    }
  }
}
function $getItem(location) {
  /* Get a jQuery object of the item at the given location */
  var root = $root[0].childNodes[0];
  for (let i = 0; i < location.length; i++) {
    if ($(root).hasClass('line')) {
      throw "no";
    }
    root = root.childNodes[location[i]];
  }
  return $(root);
}
function focusAt(loc) {
  $getItem(loc).find('input').focus();
  // For whatever reason, this only works in a timeout
  setTimeout(() => {
    if (document.activeElement.value) {
      document.activeElement.selectionStart = document.activeElement.selectionEnd = document.activeElement.value.length
    }
  }, 1);
}

// Most recently focused proposition input
var $recentInput = null;

function $makeLine(line, lineno, scope, linenos, depth, i) {
  var sidetext = "";
  if (line.kind !== kindEmpty) {
    var errored = false;
    try {
      sidetext = justify(line, scope, linenos, i);
    } catch (errorReason) {
      errored = true;
      sidetext = errorReason;
    }
  }

  let $r = $('<p>', {class: "line"})
    .append( $('<span>', {class: "lineno"}).text(lineno) )
    .append( $('<span>', {class: "input-group"})
      .append( $('<input>', {class: "input"}).val(line.sourcecode)
        .on('focus', e => { $recentInput = $(e.target); })
        .on('input', textboxChangeHandler)
        .on('keydown', e => {
          // Only call meta handler on backspace if empty input
          if (e.key === "Backspace") {
            if (e.target.value === "" || e.altKey) {
              metaKeyHandler(e);
              return false; // Otherwise, would backspace on previous line
            }
          } else {
            metaKeyHandler(e);
          }
          return !["Tab"].includes(e.key);
        }) )
      .append( $('<span>', {class: "overlay"}).text(prettify(line.sourcecode)) ) )
    .append( $('<p>', {class: "proof"}).text(sidetext) );

  if (errored) {
    $r.addClass("invalid");
  }
  return $r;
}
function $makeContext(lines) {
  var $el = $('<div>', {class: "context"});
  for (let i = 0; i < lines.length; i++) {
    $el.append(lines[i]);
  }
  return $el;
}

class Proof {
  constructor(items) {
    // List of items: either a line (AST node) or a subproof (`Proof`)
    this.items = items || [];
  }

  static recSize(item) {
    /* 1 for a line; recursive size for a Proof */
    if (item instanceof Proof) {
      return item.items.map(Proof.recSize).reduce((a, c) => a + c, 0);
    } else {
      return 1;
    }
  }

  render(renderProof = $makeContext, renderProp = $makeLine, initScope = [], initLinenos = [1], depth = 0) {
    /* `initScope` is all the lines that can be used as proof
       `initlinenos` is a parallel list of the line nubers for the scope
       Returns a jQuery entity */
    return renderProof(this.items.map((item, i) => {
      let scope = initScope.concat(this.items.slice(0, i));
      let linenos = initLinenos.concat(Array.from(Array(i),
          (_, j) => initLinenos[initLinenos.length - 1] + this.items.slice(0, j + 1).map(Proof.recSize).reduce((a, c) => a + c, 0)
      ));
      let lineno = linenos[linenos.length - 1];
      return item instanceof Proof ? item.render(renderProof, renderProp, scope, linenos, depth + 1)
                                   : renderProp(item, lineno, scope, linenos, depth + 1, i);
    }), depth);
  }

  get conclusion() {
    /* Get the last nonempty propositioin in the proof.
       If there is none (i.e., the last item is a Proof), return an empty node */
    for (let i = this.items.length - 1; i >= 0; i--) {
      let item = this.items[i];
      if (item instanceof Proof) {
        return Proposition.newEmpty("");
      } else if (item.kind !== "empty") {
        return item;
      }
    }
    return Proposition.newEmpty("");
  }
  get assumption() {
    if (this.items.length === 0) {
      return null;
    }
    return this.items[0];
  }

  mapItem(location, fun) {
    /* Apply some mapping function to a particular item. */
    if (location.length === 0) {
      let newThis = fun(this);
      // Note that this must, to be proper, copy all attributes over
      this.items = newThis.items;
      return;
    }

    var items = this.items;
    for (let i = 0; i < location.length - 1; i++) {
      console.assert(items[location[i]] instanceof Proof);
      items = items[location[i]].items;
    };
    items[location[location.length - 1]] = fun(items[location[location.length - 1]]);
  }
  getItem(location) {
    var r;
    this.mapItem(location, item => {
      r = item;
      return item;
    });
    return r;
  }

  get maxLineno() {
    var items = this.items;
    var result = 0;
    while (items.length > 0) {
      // Add the number of lines
      result += items.filter(i => !(i instanceof Proof)).length;
      // And recur onto proofs
      items = items.filter(i => i instanceof Proof).map(p => p.items)
                   .reduce((acc, val) => acc.concat(val), []);  // flatten
    }
    return result;
  }
  locToLineno(loc, items = this.items) {
    /* Fails silently */
    if (loc.length === 1) {
      return items.slice(0, loc[0] + 1).map(Proof.recSize).reduce((a, c) => a + c, 0);
    }
    return items.slice(0, loc[0]).map(Proof.recSize).reduce((a, v) => a + v, 0) +
           this.locToLineno(loc.slice(1), items[loc[0]].items);
  }
  linenoToLoc(lineno, items = this.items) {
    /* Converts a line number to a location, or returns null for an invalid line number. */
    var cumLineno = 0;
    for (let i = 0; i < items.length; i++) {
      let item = items[i];
      if (item instanceof Proof) {
        let size = Proof.recSize(item);
        if (cumLineno + size >= lineno) {
          return [i].concat(this.linenoToLoc(lineno - cumLineno, item.items));
        } else {
          cumLineno += size;
        }
      } else { // line
        cumLineno += 1;
      }
      if (cumLineno == lineno) {
        return [i];
      }
    }
  }

  prevLineno(lineno) {
    return Math.max(1, lineno - 1);
  }
  nextLineno(lineno) {
    return Math.min(lineno + 1, this.maxLineno);
  }

  prevLocation(loc) {
    return this.linenoToLoc(this.prevLineno(this.locToLineno(loc)))
  }
  nextLocation(loc) {
    return this.linenoToLoc(this.nextLineno(this.locToLineno(loc)))
  }
}

var proof;
let urlProof = getProofFromUrl();
if (urlProof) {
  try {
    proof = deserialize(urlProof);
  } catch (e) {
    console.log("Error parsing url proof", e);
    proof = new Proof([parse("")]);
  }
} else {
  proof = new Proof([parse("")]);
}

function show() {
  $root.empty().append(proof.render());
  storeProofInUrl(serialize(proof));
}
show();
focusAt([0]);

$('.literal').click(e => {
  var val = $(e.target).html();
  switch(val) {
    case "&gt;": val = ">"; break;
    case "&amp;": val = "&"; break;
  }
  if ($recentInput) {
    let loc = getLocation($recentInput[0].parentNode.parentNode);
    let selStart = $recentInput[0].selectionStart;
    let selEnd = $recentInput[0].selectionEnd;
    proof.mapItem(loc, line => parse($recentInput.val().slice(0, selStart) + val + $recentInput.val().slice(selEnd)));
    show();
    $getItem(loc).find('input').focus();
    document.activeElement.selectionStart = document.activeElement.selectionEnd = selStart + 1;
  }
});

$('#astext').click(() => {
  let t = asText(proof);
  $('#outputtext').val(t).addClass('filled');
  $('#outputtext').select();
});

function textboxChangeHandler(ev) {
  /* Handles input to the textboxes */
  let selStart = ev.target.selectionStart;
  let selEnd = ev.target.selectionEnd;
  let focusLoc = getLocation(ev.target.parentNode.parentNode);
  // Update proof with new data
  proof.mapItem(focusLoc, line => parse(ev.target.value));
  show();
  focusAt(focusLoc);
  // Need a timeout because of a chromium bug
  setTimeout(() => {
    $getItem(focusLoc).find('input')[0].setSelectionRange(selStart, selEnd);
  }, 0);
}

$('#enter').click(() => metaKeyHandler({ key: "Enter", target: $recentInput[0] }));
$('#tab').click(() => metaKeyHandler({ key: "Tab", target: $recentInput[0] }));
$('#shift-tab').click(() => metaKeyHandler({ key: "Tab", shiftKey: true, target: $recentInput[0] }));
$('#alt-backspace').click(() => metaKeyHandler({ key: "Backspace", altKey: true, target: $recentInput[0] }));

function flash($el) {
  $el.removeClass("flash");
  // A tad bit hacky
  setTimeout(() => $el.addClass("flash"), 15);
}

function metaKeyHandler(ev) {
  /* Handles "meta keys" which have context to the proof as a whole */
  let $target = $(ev.target);
  if ($target.hasClass("input")) {
    let focusLoc = getLocation(ev.target.parentNode.parentNode);
    switch (ev.key) {
      case "Enter":
        proof.mapItem(
          // Get the parent context
          focusLoc.slice(0, focusLoc.length - 1),
          // And append a new line
          proof => {
            let blankLine = parse("");
            proof.items.splice(focusLoc[focusLoc.length - 1] + 1, 0, blankLine);
            return proof;
          },
        );
        show();
        focusAt(proof.nextLocation(focusLoc));
        break;

      case "ArrowDown":
        focusAt(proof.nextLocation(focusLoc));
        break;

      case "ArrowUp":
        focusAt(proof.prevLocation(focusLoc));
        break;

      case "Tab":
        if (ev.shiftKey) {
          // Only allow shift+tab on the last line of a context
          if (focusLoc[focusLoc.length - 1] + 1 !== proof.getItem(focusLoc.slice(0, focusLoc.length - 1)).items.length) {
            // If used wrongly, flash a warning
            flash($('#end-assumption-restriction'));
          } else {
            let focusLineno = proof.locToLineno(focusLoc);
            // Remove the line from the end of its parent proof
            // And append it to the grandparent proof
            let line = proof.getItem(focusLoc);
            // Actually append it first so that don't have to account for an index change
            proof.mapItem(focusLoc.slice(0, focusLoc.length - 2), grandparentProof => {
              grandparentProof.items.splice(focusLoc[focusLoc.length - 2] + 1, 0, line);
              return grandparentProof;
            });
            // Now remove it from the parent proof
            proof.mapItem(focusLoc.slice(0, focusLoc.length -1), parentProof => {
              parentProof.items.splice(focusLoc[focusLoc.length - 1], 1);
              return parentProof;
            });
            // It's possible that this left a proof with no items in it; remove it if that's the case
            proof.mapItem(focusLoc.slice(0, focusLoc.length - 2), grandparentProof => {
              let parentI = focusLoc[focusLoc.length - 2];
              let parentProof = grandparentProof.items[parentI];
              if (parentProof.items.length === 0) {
                grandparentProof.items.splice(parentI, 1);
              }
              return grandparentProof;
            });
            show();
            focusAt(proof.linenoToLoc(focusLineno));
          }
        } else { // tab, no shift
          // Do not allow on first line of parent context
          if (focusLoc[focusLoc.length - 1] === 0) {
            // If attempted, flash a warning
            flash($('#new-assumption-restriction'));
          } else {
            proof.mapItem(focusLoc, line => {
              var pf = new Proof();
              pf.items.push(line);
              return pf;
            });
            show();
            focusAt(focusLoc);
          }
        }
        break;

      case "Backspace":
        if (ev.altKey) {
          proof = new Proof([parse("")]);
          show();
          focusAt([0]);
        } else {
          // Assumption: Line is empty. Ensured because this function is only called if the line is empty.
          // Do not delete the line if it's the only line
          if (!(focusLoc.length === 1 && focusLoc[0] === 0 && proof.items.length === 1)) {
            let prevLoc = proof.prevLocation(focusLoc);
            proof.mapItem(
              focusLoc.slice(0, focusLoc.length - 1),
              proof => {
                proof.items.splice(focusLoc[focusLoc.length - 1], 1);
                return proof;
              }
            );

            // If that line was the only line in its proof,
            if (proof.getItem(focusLoc.slice(0, focusLoc.length - 1)).items.length === 0) {
              // then an empty proof was left and we should remove it
              proof.mapItem(
                focusLoc.slice(0, focusLoc.length - 2),
                proof => {
                  proof.items.splice(focusLoc[focusLoc.length - 2], 1);
                  return proof;
                }
              );
            }
            show();
            focusAt(prevLoc);
          }
        }
        break;
    }
  }
}

