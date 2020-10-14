'use strict';

// requires: parse.js

/* Contains the code for seralizing and deserializing proofs,
   as well as storing and retrieving proofs to/from the URL. */

const LEFT = "$";
const RIGHT = "!";
const SEP = ";";
const ANY_RE = /[;\$!]/g;

function normalize(code) {
  const replacements = {
    "-": "~",
    "!": "~",
    ".": "*",
    "&": "*",
    "|": "+",
    "v": "+",
    "\\": "V",
    "@": "E",
    "_": "#",
  };

  var result = "";
  for (let i = 0; i < code.length; i++) {
    var c = code[i];
    c = replacements[c] || c;
    result += c;
  }
  return result;
}

function serialize(item) {
  if (item instanceof Proof) {
    return LEFT + item.items.map(serialize).join(SEP) + RIGHT;
  } else { // Proposition
    return normalize(item.sourcecode);
  }
}

function makeTree_(code) {
  if (code === "") {
    throw "no sir";
  }
  if (code === LEFT + RIGHT) {
    return [[""], ""];
  }
  if (code[0] === LEFT) {
    var rest = code.slice(1);
    var vals = [];
    while (true) {
      var val;
      [val, rest] = makeTree_(rest);
      vals.push(val);
      if (rest[0] === RIGHT) {
        rest = rest.slice(1);
        break;
      }
      rest = rest.slice(1);
    }
    return [vals, rest];
  } else {
    let i = code.search(ANY_RE);
    return [code.slice(0, i), code.slice(i)];
  }
}
function makeTree(code) {
  let [tree, rest] = makeTree_(code);
  if (rest !== "") {
    throw "Nonempty rest";
  }
  return tree;
}
function deserializeTree(item) {
  if (item instanceof Array) {
    return new Proof(item.map(deserializeTree));
  } else {
    return parse(item);
  }
}
function deserialize(code) {
  if (code === null) {
    return null;
  }
  return deserializeTree(makeTree(code));
}

function storeProofInUrl(serialized) {
  window.history.replaceState("", "", window.location.pathname + "?proof=" + encodeURI(serialized));
}
function getProofFromUrl() {
  /* Return the serialized proof from the URL.
     If the URL is malformed, return null. */
  let url = window.location.href;
  let i = url.indexOf("?proof=");
  if (i !== -1) {
    return decodeURI(url.slice(i + 7));
  } else {
    return null;
  }
}
