'use strict';

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
  if (/^[a-zA-Z]$/.test(code[0]) && code[0] !== "E" && code[0] !== "V" && code[0] !== "v") {
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
function parseDisjunction(code) { return parseBinaryOp(code, ["|", "v"], DISJUNCTION, parseImplication); }
function parseConjunction(code) { return parseBinaryOp(code, [".", "&", "^"], CONJUNCTION, parseDisjunction); }

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
    "^": CONJUNCTION,
    "|": DISJUNCTION,
    "v": DISJUNCTION,
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
