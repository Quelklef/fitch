'use strict';

/*
TODO:
- Complain if a name variable is used without declaration
- For some reason, UpArrow moves cursor to left and DownArrow to right
- Rename astEq, since it doesn't actually test for equality
  Additionally, the `decl` hack with astEq means that someone can prove
  a `decl` as a proposition via the rules. This should not be allowed.
  Could be fixed by rejecting all declarations not on the first line
  of a proof.
*/

/*
POSSIBLE FEATURES:
- Highlight selected line
- Highlight direct dependencies of selected line
  (i.e. the lines referenced in its justification)
- Highlight lines that depend on an invalid line,
  or lines that depend on one of these lines
*/

const IMPLICATION = "&rarr;";
const BICONDITIONAL = "&harr;";
const NEGATION = "&not;";
const CONJUNCTION = "&and;";
const DISJUNCTION = "&or;";
const BOTTOM = "&perp;";

const FORALL = "&forall;";
const EXISTS = "&exist;";
const VAR_OPEN = "[";
const VAR_CLOSE = "]";

const OPEN = "(";
const CLOSE = ")";

const unaryOps = [NEGATION];
const existentialOps = [FORALL, EXISTS];
const binaryOps = [IMPLICATION, BICONDITIONAL, CONJUNCTION, DISJUNCTION];

function parseName(code) {
  /* Names must be single-char */
  // Cannot use E and V as they're for exists/forall
  if (/^[a-zA-Z]$/.test(code[0]) && code[0] !== "E" && code[0] !== "V") {
    return [ {kind: "name", name: code[0], sourcecode: code[0]}
           , code.slice(1)
           ];
  } else {
    throw "lmao no";
  }
}

function parseUnaryOp(code, operators, kind) {
  if (!operators.includes(code[0])) {
    throw "Expected " + operator;
  }

  let [body, rest] = parseAtom(code.substring(1));
  return [ {kind: kind, body: body, sourcecode: code.slice(0, code.length - rest.length)}
    , rest
    ];
}

function parsePredicate(code) {
  var name, rest;
  [name, rest] = parseName(code);
  if (rest[0] !== "(") {
    throw "badddd";
  }
  rest = rest.slice(1);
  var args = [];
  while (true) {
    var arg;
    [arg, rest] = parseName(rest);
    args.push(arg);
    if (rest[0] === ")") {
      rest = rest.slice(1);
      break;
    } else if (rest[0] === ",") {
      rest = rest.slice(1);
    } else {
      throw "!!";
    }
  }
  return [ {kind: "predicate", target: name, args: args, sourcecode: code.substring(0, code.length - rest.length)}
         , rest
         ];
}

function parseExistentialOp(code, operator, kind) {
  if (code[0] !== operator) {
    throw "got damn";
  }
  var rest = code.slice(1);
  var name;
  [name, rest] = parseName(rest);
  var body;
  [body, rest] = parseSimpleProp(rest);
  return [ {kind: kind, name: name, body: body, sourcecode: code.slice(0, code.length - rest.length)}
         , rest
         ];
}

function parseAtom(code) {
  try {
    return parsePredicate(code);
  } catch(e) { }

  switch(code[0]) {
    case "(":
      let [ast, rest] = parseProposition(code.slice(1));
      if (rest[0] !== ")") {
        throw "bad!";
      }
      ast.sourcecode = code.slice(0, ast.sourcecode.length + 2);
      return [ast, rest.slice(1)];
      break;
    case "-":
    case "~":
    case "!":
      return parseUnaryOp(code, code[0], NEGATION);
      break;
    case "\\":
    case "V":
      return parseExistentialOp(code, code[0], FORALL);
      break;
    case "@":
    case "E":
      return parseExistentialOp(code, code[0], EXISTS);
      break;
  }
  if (code[0] === "_") {
    return [ {kind: BOTTOM, sourcecode: "_"}
           , code.slice(1)
           ];
  } else {
    return parseName(code);
  }
}

function parseBinaryOp(code, operators, kind, parseFunc) {
  /* Attempts to parse a binary operator.
     If the operator is not present, equivalent to `parseFunc(code)`. */
  var lhs, connective, rhs, rest;
  [lhs, rest] = parseFunc(code);
  if (rest === "" || !operators.includes(rest[0])) {
    return [lhs, rest];
  }
  [connective, rest] = [rest[0], rest.substring(1)];
  [rhs, rest] = parseFunc(rest);
  return [ {kind: kind, lhs: lhs, rhs: rhs, sourcecode: code.slice(0, code.length - rest.length)}
    , rest
    ];
}

function parseBiconditional(code) { return parseBinaryOp(code, ["="], BICONDITIONAL, parseAtom); }
function parseImplication(code) { return parseBinaryOp(code, [">"], IMPLICATION, parseBiconditional); }
function parseDisjunction(code) { return parseBinaryOp(code, ["|"], DISJUNCTION, parseImplication); }
function parseConjunction(code) { return parseBinaryOp(code, [".", "&"], CONJUNCTION, parseDisjunction); }

function parseSimpleProp(code) {
  /* Proposition which is not a variable declaration */
  return parseConjunction(code);
}

function parseVarDecl(code) {
  var body, node, rest;
  if (code[0] !== "[") {
    throw "Expected [";
  }
  rest = code.slice(1);
  [node, rest] = parseName(rest);
  if (rest[0] !== "]") {
    throw "Expected ]";
  }
  rest = rest.slice(1);
  try {
    [body, rest] = parseSimpleProp(rest);
  } catch (e) {
    body = {kind: "empty", sourcecode: ""};
  }
  return [ {kind: "decl", name: node, body: body, sourcecode: code.slice(0, code.length - rest.length)}
         , rest
         ];
}

function parseProposition(code) {
  if (code[0] === "[") {
    return parseVarDecl(code);
  }

  return parseSimpleProp(code);
}

function prettifyChar(char) {
  const mapping = {
    ">": IMPLICATION,
    "=": BICONDITIONAL,
    "-": NEGATION,
    "~": NEGATION,
    "!": NEGATION,
    ".": CONJUNCTION,
    "&": CONJUNCTION,
    "|": DISJUNCTION,
    "(": OPEN,
    ")": CLOSE,
    "_": BOTTOM,
    "\\": FORALL,
    "V": FORALL,
    "@": EXISTS,
    "E": EXISTS,
  };

  if (char in mapping) {
    return mapping[char];
  }

  if (/^[a-zA-Z]$/i.test(char)) {
    return char;
  }

  throw "Invalid char";
}

function lex(code) {
  /* Does not actually lex into tokens. Just a lightweight
     transformation from source to parsable formats */
   return Array.from(code)
    .map(char => {
      if (char === " ") {
        return "";
      }
      return char;
    })
    .join("");
}

function prettify(code) {
  /* Transforms source code into code to be displayed */
  return Array.from(code)
    .map(char => {
      try {
        return prettifyChar(char);
      } catch (e) {
        return char;
      }
    })
    .join("");
}

function parse(code) {
  /* Parse a proposition. If it's empty, return special node {kind: "empty"}.
     Otherwise, return the AST, unless there's a syntax error;
     then, return special node {kind: "invalid"} */
  const invalid = {kind: "invalid", sourcecode: code};
  let toks = lex(code);
  if (toks === "") {
    return {kind: "empty", sourcecode: code};
  }

  var ast, rest;
  try {
    [ast, rest] = parseProposition(toks);
  } catch (e) {
    return invalid;
  }
  if (rest !== "") {
    return invalid;
  }
  return ast;
}

function arrEq(ar0, ar1, eq) {
  eq = eq || ((a, b) => a == b);
  if (ar0.length !== ar1.length) {
    return false;
  }
  for (let i = 0; i < ar0.length; i++) {
    if (!eq(ar0[i], ar1[i])) {
      return false;
    }
  }
  return true;
}
function astEq(node0, node1) {
  /* Slight misnomer. Takes two ITEMS and returns true iff
     they are both lines (AST nodes) and equal.
     Note that this has special behaviour around declarations, in that
     it compares the BODY of a declaration to another ndoe. */
  if (node0 === null || node1 === null) return false;
  if (node0 instanceof Proof || node1 instanceof Proof) return false;
  if (node0.kind === "decl") return astEq(node0.body, node1);
  if (node1.kind === "decl") return astEq(node0, node1.body);
  if (node0.kind !== node1.kind) return false;
  let kind = node0.kind;
  if (kind === "invalid" || kind === "empty") return false;
  return false
    || (kind === BOTTOM)
    || (kind === "name" && node0.name === node1.name)
    || (kind === "decl" && astEq(node0.body, node1.body))
    || (kind === "predicate" && astEq(node0.target, node1.target) && arrEq(node0.args, node1.args, astEq))
    || (unaryOps.includes(kind) && astEq(node0.body, node1.body))
    || (existentialOps.includes(kind) && astEq(node0.name, node1.name) && astEq(node0.body, node1.body))
    || (binaryOps.includes(kind) && astEq(node0.lhs, node1.lhs) && astEq(node0.rhs, node1.rhs))
    ;
}

function justifyReiteration(line, scope, linenos) {
  for (let i = 0; i < scope.length; i++) {
    if (astEq(scope[i], line)) {
      return "R:" + linenos[i];
    }
  }
}
function justifyConjunctionIntroduction(goal, scope, linenos) {
  if (goal.kind !== CONJUNCTION) {
    return null;
  }
  var lhsPf = null;
  var rhsPf = null;
  for (let i = 0; i < scope.length; i++) {
    let line = scope[i];
    if (astEq(line, goal.lhs)) {
      lhsPf = i;
    }
    if (astEq(line, goal.rhs)) {
      rhsPf = i;
    }
    if (lhsPf !== null && rhsPf !== null) {
      return CONJUNCTION + "I:" + linenos[lhsPf] + "," + linenos[rhsPf];
    }
  }
}
function justifyConjunctionElimination(goal, scope, linenos) {
  for (let i = 0; i < scope.length; i++) {
    let line = scope[i]
    if (line.kind === CONJUNCTION && (astEq(line.lhs, goal) || astEq(line.rhs, goal))) {
      return CONJUNCTION + "E:" + linenos[i];
    }
  }
}
function justifyDisjunctionIntroduction(goal, scope, linenos) {
  if (goal.kind !== DISJUNCTION) {
    return null;
  }
  for (let i = 0; i < scope.length; i++) {
    let line = scope[i];
    if (astEq(goal.lhs, line) || astEq(goal.rhs, line)) {
      return DISJUNCTION + "I:" + linenos[i];
    }
  }
}
function justifyDisjunctionElimination(goal, scope, linenos) {
  // Get proofs with the desired conclusion
  for (let i = 0; i < scope.length; i++) {
    let line = scope[i];
    if (line instanceof Proof || line.kind !== DISJUNCTION) {
      continue;
    }
    for (let j = 0; j < scope.length; j++) {
      let jproof = scope[j];
      if (!(jproof instanceof Proof) || !astEq(jproof.conclusion, goal)) {
        continue;
      }
      for (let k = 0; k < scope.length; k++) {
        let kproof = scope[k];
        if (!(kproof instanceof Proof) || !astEq(kproof.conclusion, goal)) {
          continue;
        }
        if (astEq(line.lhs, jproof.assumption) && astEq(line.rhs, kproof.assumption)
         || astEq(line.rhs, jproof.assumption) && astEq(line.lhs, kproof.assumption)) {
          return DISJUNCTION + "E:" + linenos[i] + "," + linenos[j] + "-" + (linenos[j+1]-1) + "," + linenos[k] + "-" + (linenos[k+1]-1);
        }
      }
    }
  }
}
function justifyImplicationIntroduction(goal, scope, linenos) {
  if (goal.kind !== IMPLICATION) {
    return null;
  }
  for (let i = 0; i < scope.length; i++) {
    let item = scope[i];
    if (item instanceof Proof
     && astEq(item.assumption, goal.lhs)
     && astEq(item.conclusion, goal.rhs)) {
      return IMPLICATION + "I:" + linenos[i] + "-" + (linenos[i+1]-1);
    }
  }
}
function justifyImplicationElimination(goal, scope, linenos) {
  for (let i = 0; i < scope.length; i++) {
    let iitem = scope[i];
    if (iitem instanceof Proof || iitem.kind !== IMPLICATION || !astEq(iitem.rhs, goal)) {
      continue;
    }
    for (let j = 0; j < scope.length; j++) {
      let jitem = scope[j];
      if (jitem instanceof Proof) {
        continue;
      }
      if (astEq(iitem.lhs, jitem)) {
        return IMPLICATION + "E:" + linenos[i] + "," + linenos[j];
      }
    }
  }
}
function justifyBiconditionalIntroducton(goal, scope, linenos) {
  if (goal.kind !== BICONDITIONAL) {
    return null;
  }
  function proofPredicate(item) { return item instanceof Proof
                                    && (astEq(item.assumption, goal.lhs) || astEq(item.conclusion, goal.lhs))
                                    && (astEq(item.assumption, goal.rhs) || astEq(item.conclusion, goal.rhs)) };
  for (let i = 0; i < scope.length; i++) {
    let iproof = scope[i];
    if (!proofPredicate(iproof)) {
      continue;
    }
    for (let j = 0; j < scope.length; j++) {
      let jproof = scope[j];
      if (!proofPredicate(jproof)) {
        continue;
      }
      if (  (astEq(iproof.assumption, goal.lhs) && astEq(iproof.conclusion, goal.rhs)
          && astEq(jproof.assumption, goal.rhs) && astEq(jproof.conclusion, goal.lhs))
         || (astEq(iproof.assumption, goal.rhs) && astEq(iproof.conclusion, goal.lhs)
          && astEq(jproof.assumption, goal.lhs) && astEq(jproof.conclusion, goal.rhs)) ) {
        return BICONDITIONAL + "I:" + linenos[i] + "-" + (linenos[i+1]-1) + "," + linenos[j] + "-" + (linenos[j+1]-1);
      }
    }
  }
}
function justifyBiconditionalElimination(goal, scope, linenos) {
  for (let i = 0; i < scope.length; i++) {
    let item = scope[i];
    if (item.kind === BICONDITIONAL && (astEq(item.lhs, goal) || astEq(item.rhs, goal))) {
      return BICONDITIONAL + "E:" + linenos[i];
    }
  }
}
function justifyBottomIntroduction(goal, scope, linenos) {
  for (let i = 0; i < scope.length; i++) {
    let item = scope[i];
    if (item.kind === CONJUNCTION &&
        ((item.lhs.kind === NEGATION && astEq(item.lhs.body, item.rhs))
      || (item.rhs.kind === NEGATION && astEq(item.rhs.body, item.lhs)))) {
      return BOTTOM + "I:" + linenos[i];
    }
  }
}
function justifyNegationIntroduction(goal, scope, linenos) {
  if (goal.kind !== NEGATION) {
    return null;
  }
  for (let i = 0; i < scope.length; i++) {
    let item = scope[i];
    if (item instanceof Proof && astEq(item.assumption, goal.body) && item.conclusion.kind === BOTTOM) {
      return NEGATION + "I:" + linenos[i] + "-" + (linenos[i+1]-1);
    }
  }
}
function justifyNegationElimination(goal, scope, linenos) {
  for (let i = 0; i < scope.length; i++) {
    let item = scope[i];
    if (item.kind === NEGATION && item.body.kind === NEGATION && astEq(item.body.body, goal)) {
      return NEGATION + "E:" + linenos[i];
    }
  }
}
function varRepl(ast, nameFrom, nameTo) {
  /* Return `ast` with the name `nameFrom` recursively replaced with `nameTo`. */
  // TODO: I fucking hate this function. It's such a bad code smell.
  switch(ast.kind) {
    case CONJUNCTION:
    case DISJUNCTION:
    case IMPLICATION:
    case BICONDITIONAL:
      return {kind: ast.kind, lhs: varRepl(ast.lhs, nameFrom, nameTo), rhs: varRepl(ast.rhs, nameFrom, nameTo), sourcecode: ast.sourcecode};
    case NEGATION:
      return {kind: ast.kind, body: varRepl(ast.body, nameFrom, nameTo), sourcecode: ast.sourcecode}
    case FORALL:
    case EXISTS:
      return {kind: ast.kind, name: astEq(ast.name, nameFrom) ? nameTo : ast.name, body: ast.body, sourcecode: ast.sourcecode};
    case "name":
      return astEq(ast, nameFrom) ? nameTo : ast;
    case "predicate":
      return {kind: ast.kind, target: ast.target, args: ast.args.map(arg => varRepl(arg, nameFrom, nameTo)), sourcecode: ast.sourcecode};
    case "decl":
      throw "cannot do variable replacement on a declaration";
    case "invalid":
    case "empty":
    case BOTTOM:
      return ast;
    default:
      throw "programmer is an idiot: " + ast.kind;
  }
}
function freeVars(ast) {
  /* Recursively collect and return all free name nodes.
     Note that this includes propositions as well as bona fide name variables. */
  switch(ast.kind) {
    case CONJUNCTION:
    case DISJUNCTION:
    case IMPLICATION:
    case BICONDITIONAL:
      return new Set([...freeVars(ast.lhs), ...freeVars(ast.rhs)]);
    case NEGATION:
      return freeVars(ast.body);
    case FORALL:
    case EXISTS:
      return new Set([ast.name, ...freeVars(ast.body)]);
    case "name":
      return new Set([ast]);
    case "predicate":
      return new Set([ast.target].concat(ast.args));
    case "decl":
      var result = freeVars(ast.body);
      result.delete(ast.name);
    case "invalid":
    case "empty":
    case BOTTOM:
      return new Set();
    default:
      throw "forgot a case... " + ast.kind;
  }
}
const RARR = "&rarr;";
function justifyForallIntroduction(goal, scope, linenos) {
  if (goal.kind !== FORALL) {
    return null;
  }
  for (let i = 0; i < scope.length; i++) {
    let proof = scope[i];
    if (proof instanceof Proof
     && proof.assumption.kind === "decl"
     && proof.assumption.body.kind === "empty"
     && astEq(varRepl(proof.conclusion, proof.assumption.name, goal.name), goal.body)) {
      return FORALL + "I:" + linenos[i] + "-" + (linenos[i+1]-1) + " [" + proof.assumption.name.name + RARR + goal.name.name + "]";
    }
  }
}
function justifyForallElimination(goal, scope, linenos) {
  let names = Array.from(freeVars(goal));
  for (let n = 0; n < names.length; n++) {
    let name = names[n];
    for (let i = 0; i < scope.length; i++) {
      let item = scope[i];
      if (item instanceof Proof || item.kind !== FORALL) {
        continue;
      }
      if (astEq(varRepl(item.body, item.name, name), goal)) {
        return FORALL + "E:" + linenos[i] + "[" + item.name.name + RARR + name.name + "]";
      }
    }
  }
}
function justifyExistsIntroduction(goal, scope, linenos) {
  if (goal.kind !== EXISTS) {
    return null;
  }
  for (let i = 0; i < scope.length; i++) {
    let item = scope[i];
    if (item instanceof Proof) {
      continue;
    }
    let names = Array.from(freeVars(item));
    for (let n = 0; n < names.length; n++) {
      let name = names[n];
      if (astEq(varRepl(item, name, goal.name), goal.body)) {
        return EXISTS + "I:" + linenos[i] + "[" + name.name + RARR + goal.name.name + "]";
      }
    }
  }
}
function justifyExistsElimination(goal, scope, linenos) {
  for (let i = 0; i < scope.length; i++) {
    let iline = scope[i];
    if (iline instanceof Proof || iline.kind !== EXISTS) {
      continue;
    }
    for (let j = 0; j < scope.length; j++) {
      let jproof = scope[j];
      if (jproof instanceof Proof
       && jproof.assumption.kind === "decl"
       && astEq(jproof.conclusion, goal)
       && astEq(iline.body, varRepl(jproof.assumption.body, jproof.assumption.name, iline.name))) {
        return EXISTS + "E:" + linenos[i] + "," + linenos[j] + "-" + (linenos[j+1]-1);
      }
    }
  }
}
function justify(line, scope, linenos, i) {
  /* Justify a line (AST Node) with all the lines of the given scope.
     The line numbers must be supplied in the parallel array `linenos`.
     `i` is the index of the line in its context */
   if (i === 0) {
     return "assumed";
   }

   let strategies =
    [ justifyReiteration
    , justifyConjunctionIntroduction
    , justifyConjunctionElimination
    , justifyDisjunctionIntroduction
    , justifyDisjunctionElimination
    , justifyImplicationIntroduction
    , justifyImplicationElimination
    , justifyBiconditionalIntroducton
    , justifyBiconditionalElimination
    , justifyBottomIntroduction
    , justifyNegationIntroduction
    , justifyNegationElimination
    , justifyForallIntroduction
    , justifyForallElimination
    , justifyExistsIntroduction
    , justifyExistsElimination
    ];

  for (let i = 0; i < strategies.length; i++) {
    let strat = strategies[i];
    let justification = strat(line, scope, linenos);
    if (justification) {
      return justification;
    }
  }

  return null;
}

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
}

function $makeLine(line, lineno, justification) {
  let sidetext =
    line.kind === "empty" ? ""
    : line.kind === "invalid" ? "malformed proposition"
    : justification === null ? "invalid step"
    : justification;
  let $r = $('<p>', {class: "line"})
    .append( $('<span>', {class: "lineno"}).html(lineno) )
    .append( $('<span>', {class: "input-group"})
      .append( $('<input>', {class: "input"}).val(line.sourcecode)
        .on('input', textboxChangeHandler)
        .on('keydown', e => {
          // Only call meta handler on backspace if empty input
          if (e.key === "Backspace") {
            if (e.target.value === "") {
              metaKeyHandler(e);
              return false; // Otherwise, would backspace on previous line
            }
          } else {
            metaKeyHandler(e);
          }
          return !["Tab"].includes(e.key);
        }) )
      .append( $('<span>', {class: "overlay"}).html(prettify(line.sourcecode)) ) )
    .append( $('<p>', {class: "proof"}).html(sidetext) );

  if (line.kind === "invalid") {
    $r.addClass("parse-error");
  }
  if (line.kind !== "empty" && justification === null) {
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

  render(initScope = [], initLinenos = [1]) {
    /* `initScope` is all the lines that can be used as proof
       `initlinenos` is a parallel list of the line nubers for the scope
       Returns a jQuery entity */
    return $makeContext(this.items.map((item, i) => {
      let scope = initScope.concat(this.items.slice(0, i));
      let linenos = initLinenos.concat(Array.from(Array(i),
          (_, j) => initLinenos[initLinenos.length - 1] + this.items.slice(0, j + 1).map(Proof.recSize).reduce((a, c) => a + c, 0)
      ));
      let lineno = linenos[linenos.length - 1];
      return item instanceof Proof ? item.render(scope, linenos)
                                   : $makeLine(item, lineno, justify(item, scope, linenos, i));
    }));
  }

  get conclusion() {
    /* Get the last nonempty propositioin in the proof.
       If there is none (i.e., the last item is a Proof), return an empty node */
    for (let i = this.items.length - 1; i >= 0; i--) {
      let item = this.items[i];
      if (item instanceof Proof) {
        return {kind: "empty", sourcecode: ""};
      } else if (item.kind !== "empty") {
        return item;
      }
    }
    return {kind: "empty", sourcecode: ""};
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

var proof = new Proof();
proof.items.push(parse(""));

function show() {
  $root.empty().append(proof.render());
}
show();
focusAt([0]);

function textboxChangeHandler(ev) {
  /* Handles input to the textboxes */
  let selStart = ev.target.selectionStart;
  let selEnd = ev.target.selectionEnd;
  let focusLoc = getLocation(ev.target.parentNode.parentNode);
  // Update proof with new data
  proof.mapItem(focusLoc, line => parse(ev.target.value));
  show();
  focusAt(focusLoc);
  document.activeElement.selectionStart = selStart;
  document.activeElement.selectionEnd = selEnd;
}

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
        break;
    }
  }
}

$('#prop-dm-or').click(() => {
  const propDeMorgansOrProof =
    new Proof([
      parse(""),
      new Proof([
        parse("-(P|Q)"),
        new Proof([
          parse("P"),
          parse("P|Q"),
          parse("(P|Q).-(P|Q)"),
          parse("_"),
        ]),
        parse("-P"),
        new Proof([
          parse("Q"),
          parse("P|Q"),
          parse("(P|Q).-(P|Q)"),
          parse("_"),
        ]),
        parse("-Q"),
        parse("-P.-Q"),
      ]),
      new Proof([
        parse("-P.-Q"),
        new Proof([
          parse("P|Q"),
          new Proof([
            parse("P"),
            parse("-P"),
            parse("P.-P"),
            parse("_"),
          ]),
          new Proof([
            parse("Q"),
            parse("-Q"),
            parse("Q.-Q"),
            parse("_"),
          ]),
          parse("_"),
        ]),
        parse("-(P|Q)")
      ]),
      parse("-(P|Q)=(-P.-Q)"),
    ]);

  proof = propDeMorgansOrProof;
  show();
  focusAt([0]);
});

$('#prop-dm-and').click(() => {
  let propDeMorgansAndProof =
    new Proof([
      parse(""),
      new Proof([
        parse("-(P.Q)"),
        new Proof([
          parse("-(-P|-Q)"),
          new Proof([
            parse("-P"),
            parse("-P|-Q"),
            parse("(-P|-Q).-(-P|-Q)"),
            parse("_"),
          ]),
          parse("--P"),
          parse("P"),
          new Proof([
            parse("-Q"),
            parse("-P|-Q"),
            parse("(-P|-Q).-(-P|-Q)"),
            parse("_"),
          ]),
          parse("--Q"),
          parse("Q"),
          parse("P.Q"),
          parse("(P.Q).-(P.Q)"),
          parse("_"),
        ]),
        parse("--(-P|-Q)"),
        parse("-P|-Q"),
      ]),
      new Proof([
        parse("-P|-Q"),
        new Proof([
          parse("P.Q"),
          new Proof([
            parse("-P"),
            parse("P"),
            parse("P.-P"),
            parse("_"),
          ]),
          new Proof([
            parse("-Q"),
            parse("Q"),
            parse("Q.-Q"),
            parse("_"),
          ]),
          parse("_"),
        ]),
        parse("-(P.Q)"),
      ]),
      parse("-(P.Q)=(-P|-Q)"),
    ]);

  proof = propDeMorgansAndProof;
  show();
  focusAt([0]);
});

$('#fol-dm-exist').click(() => {
  const folDeMorgansExistsProof =
    new Proof([
      new Proof([
        parse("-@xP(x)"),
        new Proof([
          parse("[a]"),
          new Proof([
            parse("P(a)"),
            parse("@xP(x)"),
            parse("(@xP(x)).-(@xP(x))"),
            parse("_"),
          ]),
          parse("-P(a)"),
        ]),
        parse("\\x-P(x)"),
      ]),
      new Proof([
        parse("\\x-P(x)"),
        new Proof([
          parse("@xP(x)"),
          new Proof([
            parse("[a]P(a)"),
            parse("-P(a)"),
            parse("P(a).-P(a)"),
            parse("_"),
          ]),
          parse("_"),
        ]),
        parse("-@xP(x)"),
      ]),
      parse("(-@xP(x))=(\\x-P(x))"),
    ]);

  proof = folDeMorgansExistsProof;
  show();
  focusAt([0]);
});

$('#fol-dm-forall').click(() => {
  const folDeMorgansForallProof =
    new Proof([
      new Proof([
        parse("-\\xP(x)"),
        new Proof([
          parse("-@x-P(x)"),
          new Proof([
            parse("[a]"),
            new Proof([
              parse("-P(a)"),
              parse("@x-P(x)"),
              parse("(@x-P(x)).-(@x-P(x))"),
              parse("_"),
            ]),
            parse("--P(a)"),
            parse("P(a)"),
          ]),
          parse("\\xP(x)"),
          parse("(\\xP(x)).-(\\xP(x))"),
          parse("_")
        ]),
        parse("--@x-P(x)"),
        parse("@x-P(x)"),
      ]),
      new Proof([
        parse("@x-P(x)"),
        new Proof([
          parse("\\xP(x)"),
          new Proof([
            parse("[a]-P(a)"),
            parse("P(a)"),
            parse("P(a).-P(a)"),
            parse("_"),
          ]),
          parse("_"),
        ]),
        parse("-\\xP(x)"),
      ]),
      parse("(-\\xP(x))=(@x-P(x))"),
    ]);

  proof = folDeMorgansForallProof;
  show();
  focusAt([0]);
});
