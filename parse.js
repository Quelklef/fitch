'use strict';

/* Proposition parsing.
   Propositional variables, predicate variables, and name variables are all conflated into
   a single kind here (name). They are semantically distinguished in justify.js */

/* -- Type definition -- */

// All the different proposition kinds
// These are arbitrary constants which need only be inequal to each other
const kindImplication = "implication";
const kindBiconditional = "biconditional";
const kindNegation = "negation";
const kindConjunction = "conjunction";
const kindDisjunction = "disjunction";
const kindEquality = "equals";
const kindBottom = "bottom";

const kindForall = "forall";
const kindExists = "exists";

const kindName = "name";
const kindPredicate = "predicate";

// These are both not "real" propositions
const kindEmpty = "empty"; // For empty lines
const kindInvalid = "invalid"; // For a parsing error

// Any proposition can have a 'declarting' attribute which should have a name node.
// This denotes that the proposition declares some name variable.

class Proposition {
  constructor(dict) {
    for (let key in dict) {
      this[key] = dict[key];
    }
    if (this.sourcecode === undefined) {
      console.log("No sourcecode!", this);
      throw "you forgot to give a sourcecode, dipshit.";
    }
  }

  static newImplication(lhs, rhs, sourcecode) {
    return new Proposition({ kind: kindImplication, lhs: lhs, rhs: rhs, sourcecode: sourcecode });
  }
  static newBiconditional(lhs, rhs, sourcecode) {
    return new Proposition({ kind: kindBiconditional, lhs: lhs, rhs: rhs, sourcecode: sourcecode });
  }
  static newNegation(body, sourcecode) {
    return new Proposition({ kind: kindNegation, body: body, sourcecode: sourcecode });
  }
  static newConjunction(lhs, rhs, sourcecode) {
    return new Proposition({ kind: kindConjunction, lhs: lhs, rhs: rhs, sourcecode: sourcecode });
  }
  static newDisjunction(lhs, rhs, sourcecode) {
    return new Proposition({ kind: kindDisjunction, lhs: lhs, rhs: rhs, sourcecode: sourcecode });
  }
  static newEquality(lhs, rhs, sourcecode) {
    return new Proposition({ kind: kindEquality, lhs: lhs, rhs: rhs, sourcecode: sourcecode });
  }
  static newBottom(sourcecode) {
    return new Proposition({ kind: kindBottom, sourcecode: sourcecode });
  }
  static newForall(name, body, sourcecode) {
    return new Proposition({ kind: kindForall, name: name, body: body, sourcecode: sourcecode });
  }
  static newExists(name, body, sourcecode) {
    return new Proposition({ kind: kindExists, name: name, body: body, sourcecode: sourcecode });
  }
  static newName(name, sourcecode) {
    return new Proposition({ kind: kindName, name: name, sourcecode: sourcecode });
  }
  static newPredicate(target, args, sourcecode) {
    return new Proposition({ kind: kindPredicate, target: target, args: args, sourcecode: sourcecode });
  }
  static newEmpty(sourcecode) {
    return new Proposition({ kind: kindEmpty, sourcecode: sourcecode });
  }
  static newInvalid(sourcecode, e = "malformed proposition") {
    return new Proposition({ kind: kindInvalid, error: e, sourcecode: sourcecode });
  }

  static concurs(prop1, prop2) {
    /* Tests if the two propositions "concur".
       This is essentially an equality check, ecept that declarations are ignored. */
    if (prop1 === null || prop2 === null) return false;
    if (!(prop1 instanceof Proposition && prop2 instanceof Proposition)) return false;
    if (prop1.kind !== prop2.kind) return false;
    let kind = prop1.kind;
    if (kind === kindInvalid || kind === kindEmpty) return false;
    return false
      || (kind === kindBottom)
      || (kind === kindName && prop1.name === prop2.name)
      || (kind === kindPredicate && Proposition.concurs(prop1.target, prop2.target) && arrEq(prop1.args, prop2.args, Proposition.concurs))
      || (unaryOps.includes(kind) && Proposition.concurs(prop1.body, prop2.body))
      || (existentialOps.includes(kind) && Proposition.concurs(prop1.name, prop2.name) && Proposition.concurs(prop1.body, prop2.body))
      || (binaryOps.includes(kind) && Proposition.concurs(prop1.lhs, prop2.lhs) && Proposition.concurs(prop1.rhs, prop2.rhs))
      ;
  }

  concurs(other) {
    return Proposition.concurs(this, other);
  }
}

function arrEq(ar0, ar1, eq) {
  eq = eq || ((a, b) => a === b);
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

const IMPLICATION = String.fromCharCode(0x2192); // →
const BICONDITIONAL = String.fromCharCode(0x2194); // ⟷
const NEGATION = String.fromCharCode(0x00AC); // ¬
const CONJUNCTION = String.fromCharCode(0x2227); // ∧
const DISJUNCTION = String.fromCharCode(0x2228); // ∨
const BOTTOM = String.fromCharCode(0x22A5); // ⊥
const EQUAL = "=";
const INEQUAL = String.fromCharCode(0x2260); // ≠

const FORALL = String.fromCharCode(0x2200); // ∀
const EXISTS = String.fromCharCode(0x2203); // ∃
const VAR_OPEN = "[";
const VAR_CLOSE = "]";

const OPEN = "(";
const CLOSE = ")";

const unaryOps = [kindNegation];
const existentialOps = [kindForall, kindExists];
const binaryOps = [kindImplication, kindBiconditional, kindConjunction, kindDisjunction, kindEquality];

/* -- Parsing -- */
/* All parsers are String -> [Proposition, String].
   The first value returned is the parsed node.
   The second value is the code left over.
   If a syntax error is reached, an exception will be thrown. */

function consume(code, string) {
  if (code.startsWith(string)) {
    return code.slice(string.length);
  } else {
    throw `expected '${string}' but got '${code}'`;
  }
}

function parseName(code) {
  /* Names must be single-char */
  let letter = code[0];
  if (!"EVv".includes(letter) && /^[a-zA-Z]$/.test(letter)) {
    return [Proposition.newName(letter, letter), code.slice(1)];
  }
  throw `invalid name '${letter}'`;
}

function parseUnaryOp(code, operators, constructor) {
  if (!operators.includes(code[0])) {
    throw `expected '${operator}' but got '${code}'`;
  }
  let [body, rest] = parseAtom(code.substring(1));
  return [constructor(body, code.slice(0, code.length - rest.length)), rest];
}

function parsePredicate(code) {
  var name, rest;
  [name, rest] = parseName(code);
  var args = [];
  while (true) {
    try {
      var arg;
      [arg, rest] = parseName(rest);
      args.push(arg);
    } catch (e) {
      break;
    }
  }
  if (rest[0] === "(") {
    throw "do not use ( for predicate";
  }
  if (args.length === 0) {
    throw "predicate requires 1 or more arguments";
  }
  return [Proposition.newPredicate(name, args, code.substring(0, code.length - rest.length)), rest];
}

function parseExistentialOp(code, operator, constructor) {
  var name, body, rest;
  rest = consume(code, operator);
  [name, rest] = parseName(rest);
  [body, rest] = parseSimpleProp(rest);
  return [constructor(name, body, code.slice(0, code.length - rest.length)), rest];
}

function parseAtom(code) {
  try {
    return parsePredicate(code);
  } catch(e) { }

  switch(code[0]) {
    case "(":
      var ast, rest;
      [ast, rest] = parseProposition(code.slice(1));
      rest = consume(rest, ")");
      ast.sourcecode = code.slice(0, ast.sourcecode.length + 2);
      return [ast, rest];

    case "-":
    case "~":
    case "!":
      return parseUnaryOp(code, code[0], Proposition.newNegation);

    case "\\":
    case "V":
      return parseExistentialOp(code, code[0], Proposition.newForall);

    case "@":
    case "E":
      return parseExistentialOp(code, code[0], Proposition.newExists);

    case "_":
    case "#":
      return [Proposition.newBottom(code[0]), code.slice(1)];

    default:
      return parseName(code);
  }
}

function parseBinaryOp(code, operators, constructor, parseFunc) {
  /* Attempts to parse a binary operator.
     If the operator is not present, equivalent to `parseFunc(code)`. */
  var lhs, connective, rhs, rest;
  [lhs, rest] = parseFunc(code);
  if (!operators.includes(rest[0])) {
    return [lhs, rest];
  }
  rest = rest.slice(1);
  [rhs, rest] = parseFunc(rest);
  return [constructor(lhs, rhs, code.slice(0, code.length - rest.length)), rest];
}

function parseInequality(code) {
  return parseBinaryOp(code, ["^"],
    (lhs, rhs, source) => Proposition.newNegation(Proposition.newEquality(lhs, rhs, source), source),
  parseAtom);
}
function parseEquality(code) {
  return parseBinaryOp(code, ["="], Proposition.newEquality, parseInequality);
}
function parseBiconditional(code) {
  return parseBinaryOp(code, ["/"], Proposition.newBiconditional, parseEquality);
}
function parseImplication(code) {
  return parseBinaryOp(code, [">"], Proposition.newImplication, parseBiconditional);
}
function parseDisjunction(code) {
  return parseBinaryOp(code, ["|", "v", "+"], Proposition.newDisjunction, parseImplication);
}
function parseConjunction(code) {
  return parseBinaryOp(code, [".", "&", "*"], Proposition.newConjunction, parseDisjunction);
}

function parseSimpleProp(code) {
  /* Proposition which is not a variable declaration */
  return parseConjunction(code);
}

function parseDeclaration(code) {
  var body, name, rest;
  rest = consume(code, "[");
  [name, rest] = parseName(rest);
  rest = consume(rest, "]");
  try {
    [body, rest] = parseProposition(rest);
  } catch (e) {
    body = Proposition.newEmpty("");
  }
  body.declaring = name;
  body.sourcecode = "[" + name.sourcecode + "]" + body.sourcecode;
  return [body, rest];
}

function parseProposition(code) {
  try {
    return parseDeclaration(code);
  } catch (e) { }
  return parseSimpleProp(code);
}

function prettify(code) {
  /* Transforms source code into code to be displayed */
  return Array.from(code)
    .map(char => ({
        ">": IMPLICATION,
        "/": BICONDITIONAL,
        "-": NEGATION,
        "~": NEGATION,
        "!": NEGATION,
        ".": CONJUNCTION,
        "&": CONJUNCTION,
        "*": CONJUNCTION,
        "|": DISJUNCTION,
        "v": DISJUNCTION,
        "+": DISJUNCTION,
        "(": OPEN,
        ")": CLOSE,
        "_": BOTTOM,
        "#": BOTTOM,
        "\\": FORALL,
        "V": FORALL,
        "@": EXISTS,
        "E": EXISTS,
        "^": INEQUAL,
      })[char] || char)
    .join("");
}

function parse(code, throwErrors=false) {
  /* Parse a proposition. If it's empty, return special node {kind: "empty"}.
     Otherwise, return the AST, unless there's a syntax error;
     then, return special node {kind: "invalid"} */
  if (code === "") {
    return Proposition.newEmpty(code);
  }

  var prop, rest;
  try {
    [prop, rest] = parseProposition(code);
  } catch (e) {
    if (throwErrors) {
      throw e;
    } else {
      // We conflate errors because they're typically not user-friendly
      return Proposition.newInvalid(code, "malformed proposition");
    }
  }

  if (rest !== "") {
    if (rest[0] === "(") {
      // We assume that this is because the user tried to do e.g. P(x).
      // This may not be the case, but it probably is.
      return Proposition.newInvalid(code, "do not use ( for predicate");
    } else {
      if (throwErrors) {
        throw `Rest is not empty; it is '${rest}'.`;
      } else {
        return Proposition.newInvalid(code, "malformed proposition");
      }
    }
  }
  return prop;
}

