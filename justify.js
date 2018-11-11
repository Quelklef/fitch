'use strict';

// requires: parse.js

/* Contains the algorithms for justifying propositions.
   Each algorithm takes 3 arguments: `line`, `scope`, and `linenos`.
   `line` is the AST Node of the proposition.
   `scope` is a list of other propositions as well as proofs in the 'scope"
   of the line, i.e., able to be used as proof.
   `linenos` is a list, parallel to `scope`, that contains the line numbers
   of lines and starting line numbers of proofs. */

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
