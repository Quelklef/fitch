'use strict';

/* This file contains the code for representing a Proof
   as text */

// requires: main.js, serialize.js, justify.js

const VERT = String.fromCharCode(0x2502); // │
const RULE = String.fromCharCode(0x2500); // ─
const TURN = String.fromCharCode(0x251C); // ├

function rep(s, n) {
  var res = "";
  for (let i = 0; i < n; i++) {
    res += s;
  }
  return res;
}

function maxDistanceRight(item, depth = 0) {
  if (item instanceof Proof) {
    return Math.max(...item.items.map(maxDistanceRight, depth + 1));
  } else {
    return depth + item.sourcecode.length;
  }
}

function separateAdjacentProofs(text) {
  /* You get no documentation because this function really shouldn't exist.
     Proof.render needs to be fixed, then astText. */
  let lines = text.split("\n");
  function depthOf(line) {
    var d = 0;
    for (let i = 0; i < line.length; i++) {
      let c = line[i];
      if (c === VERT || c === TURN) d++;
      else break;
    }
    return d;
  }
  var res = [];
  for (let i = 0; i < lines.length; i++) {
    res.push(lines[i]);
    if ((lines[i+2] || "").includes(RULE) && depthOf(lines[i+2]) === depthOf(lines[i])) {
      res.push(rep(VERT, depthOf(lines[i]) - 1));
    }
  }
  return res.join("\n");
}

function asText(proof) {
  let r = maxDistanceRight(proof) + 10;
  return proof.render(
    (lines, depth) =>
      separateAdjacentProofs([lines[0], rep(VERT, depth) + TURN + rep(RULE, 10)].concat(lines.slice(1)).join("\n")),
    (line, lineno, scope, linenos, depth, i) => {
      var justif;
      try {
        justif = justify(line, scope, linenos, i);
      } catch (e) {
        justif = e;
      }
      let res = rep(VERT, depth) + " " + lineno + ": " + prettify(line.sourcecode);
      res += rep(" ", r - res.length) + justif;
      return res;
    }
  );
}
