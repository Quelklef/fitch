'use strict';

/* -- Type definition -- */

// All the different proposition kinds
const kindImplication = "implication";
const kindBiconditional = "biconditional";
const kindNegation = "negation";
const kindConjunction = "conjunction";
const kindDisjunction = "disjunction";
const kindBottom = "bottom";

const kindForall = "forall";
const kindExists = "exists";
// Declarations like `[a]P(a)`.
// These propositions are treated specially.
const kindDeclaration = "declaration";

const kindName = "name";
const kindPredicate = "predicate";

// These are both not "real" propositions
const kindEmpty = "empty"; // For empty lines
const kindInvalid = "invalid"; // For a parsing error

class Proposition {
  constructor(dict) {
    for (let key in dict) {
      this[key] = dict[key];
    }
    if (this.sourcecode === undefined) {
      console.log("No sourcecode!", this);
      throw "You forgot to give a sourcecode, dipshit.";
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
  static newBottom(sourcecode) {
    return new Proposition({ kind: kindBottom, sourcecode: sourcecode });
  }
  static newForall(name, body, sourcecode) {
    return new Proposition({ kind: kindForall, name: name, body: body, sourcecode: sourcecode });
  }
  static newExists(name, body, sourcecode) {
    return new Proposition({ kind: kindExists, name: name, body: body, sourcecode: sourcecode });
  }
  static newDeclaration(name, body, sourcecode) {
    return new Proposition({ kind: kindDeclaration, name: name, body: body, sourcecode: sourcecode });
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
  static newInvalid(sourcecode) {
    return new Proposition({ kind: kindInvalid, sourcecode: sourcecode });
  }

  static equals(prop1, prop2) {
    /* Note that this has special behaviour around declarations, in that
       it compares the BODY of a declaration to anprop2 Proposition. */
    if (prop1 === null || prop2 === null) return false;
    if (!(prop1 instanceof Proposition && prop2 instanceof Proposition)) return false;
    if (prop1.kind === kindDeclaration) return Proposition.equals(prop1.body, prop2);
    if (prop2.kind === kindDeclaration) return Proposition.equals(prop1, prop2.body);
    if (prop1.kind !== prop2.kind) return false;
    let kind = prop1.kind;
    if (kind === kindInvalid || kind === kindEmpty) return false;
    return false
      || (kind === kindBottom)
      || (kind === kindName && prop1.name === prop2.name)
      || (kind === kindPredicate && Proposition.equals(prop1.target, prop2.target) && arrEq(prop1.args, prop2.args, Proposition.equals))
      || (unaryOps.includes(kind) && Proposition.equals(prop1.body, prop2.body))
      || (existentialOps.includes(kind) && Proposition.equals(prop1.name, prop2.name) && Proposition.equals(prop1.body, prop2.body))
      || (binaryOps.includes(kind) && Proposition.equals(prop1.lhs, prop2.lhs) && Proposition.equals(prop1.rhs, prop2.rhs))
      ;
  }

  equals(other) {
    return Proposition.equals(this, other);
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

const unaryOps = [kindNegation];
const existentialOps = [kindForall, kindExists];
const binaryOps = [kindImplication, kindBiconditional, kindConjunction, kindDisjunction];

/* -- Parsing -- */
/* All parsers are String -> [Proposition, String].
   The first value returned is the parsed node.
   The second value is the code left over.
   If a syntax error is reached, an exception will be thrown. */

function consume(code, string) {
  if (code.startsWith(string)) {
    return code.slice(string.length);
  } else {
    throw `Expected '${string}' but got '${code}'`;
  }
}

function parseName(code) {
  /* Names must be single-char */
  let letter = code[0];
  if (!"EVv".includes(letter) && /^[a-zA-Z]$/.test(letter)) {
    return [Proposition.newName(letter, letter), code.slice(1)];
  }
  throw `Invalid name '${letter}'`;
}

function parseUnaryOp(code, operators, constructor) {
  if (!operators.includes(code[0])) {
    throw `Expected '${operator}' but got '${code}'`;
  }
  let [body, rest] = parseAtom(code.substring(1));
  return [constructor(body, code.slice(0, code.length - rest.length)), rest];
}

function parsePredicate(code) {
  var name, rest;
  [name, rest] = parseName(code);
  rest = consume(rest, "(");
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

function parseBiconditional(code) {
  return parseBinaryOp(code, ["="], Proposition.newBiconditional, parseAtom);
}
function parseImplication(code) {
  return parseBinaryOp(code, [">"], Proposition.newImplication, parseBiconditional);
}
function parseDisjunction(code) {
  return parseBinaryOp(code, ["|", "v"], Proposition.newDisjunction, parseImplication);
}
function parseConjunction(code) {
  return parseBinaryOp(code, [".", "&", "^"], Proposition.newConjunction, parseDisjunction);
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
    [body, rest] = parseSimpleProp(rest);
  } catch (e) {
    body = Proposition.newEmpty("");
  }
  return [Proposition.newDeclaration(name, body, code.slice(0, code.length - rest.length)), rest];
}

function parseProposition(code) {
  try {
    return parseDeclaration(code);
  } catch (e) { }
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
   return Array.from(code).filter(char => char !== " ").join("");
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
  let toks = lex(code);
  if (toks === "") {
    return Proposition.newEmpty("");
  }

  var ast, rest;
  try {
    [ast, rest] = parseProposition(toks);
  } catch (e) {
    console.log(e);
    return Proposition.newInvalid(code);
  }
  if (rest !== "") {
    return Proposition.newInvalid(code);
  }
  return ast;
}
