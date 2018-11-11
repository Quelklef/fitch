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
    if (scope[i].equals(line)) {
      return "R:" + linenos[i];
    }
  }
}
function justifyConjunctionIntroduction(goal, scope, linenos) {
  if (goal.kind !== kindConjunction) {
    return null;
  }
  var lhsPf = null;
  var rhsPf = null;
  for (let i = 0; i < scope.length; i++) {
    let line = scope[i];
    if (line.equals(goal.lhs)) {
      lhsPf = i;
    }
    if (line.equals(goal.rhs)) {
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
    if (line.kind === kindConjunction && (line.lhs.equals(goal) || line.rhs.equals(goal))) {
      return CONJUNCTION + "E:" + linenos[i];
    }
  }
}
function justifyDisjunctionIntroduction(goal, scope, linenos) {
  if (goal.kind !== kindDisjunction) {
    return null;
  }
  for (let i = 0; i < scope.length; i++) {
    let line = scope[i];
    if (goal.lhs.equals(line) || goal.rhs.equals(line)) {
      return DISJUNCTION + "I:" + linenos[i];
    }
  }
}
function justifyDisjunctionElimination(goal, scope, linenos) {
  // Get proofs with the desired conclusion
  for (let i = 0; i < scope.length; i++) {
    let line = scope[i];
    if (line instanceof Proof || line.kind !== kindDisjunction) {
      continue;
    }
    for (let j = 0; j < scope.length; j++) {
      let jproof = scope[j];
      if (!(jproof instanceof Proof) || !jproof.conclusion.equals(goal)) {
        continue;
      }
      for (let k = 0; k < scope.length; k++) {
        let kproof = scope[k];
        if (!(kproof instanceof Proof) || !kproof.conclusion.equals(goal)) {
          continue;
        }
        if (line.lhs.equals(jproof.assumption) && line.rhs.equals(kproof.assumption)
         || line.rhs.equals(jproof.assumption) && line.lhs.equals(kproof.assumption)) {
          return DISJUNCTION + "E:" + linenos[i] + "," + linenos[j] + "-" + (linenos[j+1]-1) + "," + linenos[k] + "-" + (linenos[k+1]-1);
        }
      }
    }
  }
}
function justifyImplicationIntroduction(goal, scope, linenos) {
  if (goal.kind !== kindImplication) {
    return null;
  }
  for (let i = 0; i < scope.length; i++) {
    let item = scope[i];
    if (item instanceof Proof
     && item.assumption.equals(goal.lhs)
     && item.conclusion.equals(goal.rhs)) {
      return IMPLICATION + "I:" + linenos[i] + "-" + (linenos[i+1]-1);
    }
  }
}
function justifyImplicationElimination(goal, scope, linenos) {
  for (let i = 0; i < scope.length; i++) {
    let iitem = scope[i];
    if (iitem instanceof Proof || iitem.kind !== kindImplication || !iitem.rhs.equals(goal)) {
      continue;
    }
    for (let j = 0; j < scope.length; j++) {
      let jitem = scope[j];
      if (jitem instanceof Proof) {
        continue;
      }
      if (iitem.lhs.equals(jitem)) {
        return IMPLICATION + "E:" + linenos[i] + "," + linenos[j];
      }
    }
  }
}
function justifyBiconditionalIntroducton(goal, scope, linenos) {
  if (goal.kind !== kindBiconditional) {
    return null;
  }
  function proofPredicate(item) { return item instanceof Proof
                                    && (item.assumption.equals(goal.lhs) || item.conclusion.equals(goal.lhs))
                                    && (item.assumption.equals(goal.rhs) || item.conclusion.equals(goal.rhs)) };
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
      if (  (iproof.assumption.equals(goal.lhs) && iproof.conclusion.equals(goal.rhs)
          && jproof.assumption.equals(goal.rhs) && jproof.conclusion.equals(goal.lhs))
         || (iproof.assumption.equals(goal.rhs) && iproof.conclusion.equals(goal.lhs)
          && jproof.assumption.equals(goal.lhs) && jproof.conclusion.equals(goal.rhs)) ) {
        return BICONDITIONAL + "I:" + linenos[i] + "-" + (linenos[i+1]-1) + "," + linenos[j] + "-" + (linenos[j+1]-1);
      }
    }
  }
}
function justifyBiconditionalElimination(goal, scope, linenos) {
  for (let i = 0; i < scope.length; i++) {
    let item = scope[i];
    if (item.kind === kindBiconditional && (item.lhs.equals(goal) || item.rhs.equals(goal))) {
      return BICONDITIONAL + "E:" + linenos[i];
    }
  }
}
function justifyBottomIntroduction(goal, scope, linenos) {
  for (let i = 0; i < scope.length; i++) {
    let item = scope[i];
    if (item.kind === kindConjunction &&
        ((item.lhs.kind === kindNegation && item.lhs.body.equals(item.rhs))
      || (item.rhs.kind === kindNegation && item.rhs.body.equals(item.lhs)))) {
      return BOTTOM + "I:" + linenos[i];
    }
  }
}
function justifyNegationIntroduction(goal, scope, linenos) {
  if (goal.kind !== kindNegation) {
    return null;
  }
  for (let i = 0; i < scope.length; i++) {
    let item = scope[i];
    if (item instanceof Proof && item.assumption.equals(goal.body) && item.conclusion.kind === kindBottom) {
      return NEGATION + "I:" + linenos[i] + "-" + (linenos[i+1]-1);
    }
  }
}
function justifyNegationElimination(goal, scope, linenos) {
  for (let i = 0; i < scope.length; i++) {
    let item = scope[i];
    if (item.kind === kindNegation && item.body.kind === kindNegation && item.body.body.equals(goal)) {
      return kindNegation + "E:" + linenos[i];
    }
  }
}
function varRepl(prop, nameFrom, nameTo) {
  /* Return `prop` with the name `nameFrom` recursively replaced with `nameTo`. */
  // Todo: I fucking hate this function. It's such a bad code smell.
  switch(prop.kind) {
    case kindConjunction:
    case kindDisjunction:
    case kindImplication:
    case kindBiconditional:
      return Object.assign(new Proposition(prop), {lhs: varRepl(prop.lhs, nameFrom, nameTo), rhs: varRepl(prop.rhs, nameFrom, nameTo)});
    case kindNegation:
      return Object.assign(new Proposition(prop), {body: varRepl(prop.body, nameFrom, nameTo)});
    case kindForall:
    case kindExists:
      return Object.assign(new Proposition(prop), {name: varRepl(prop.name, nameFrom, nameTo)});
    case kindName:
      return prop.equals(nameFrom) ? nameTo : prop;
    case kindPredicate:
      return Object.assign(new Proposition(prop), {args: prop.args.map(arg => varRepl(arg, nameFrom, nameTo))});
    case kindDeclaration:
      throw "Cannot do variable replacement on a declaration";
    case kindInvalid:
    case kindEmpty:
    case kindBottom:
      return prop;
    default:
      throw "Missed case: " + prop.kind;
  }
}
function freeVars(ast) {
  /* Recursively collect and return all free name nodes.
     Note that this includes propositions as well as bona fide name variables. */
  switch(ast.kind) {
    case kindConjunction:
    case kindDisjunction:
    case kindImplication:
    case kindBiconditional:
      return new Set([...freeVars(ast.lhs), ...freeVars(ast.rhs)]);
    case kindNegation:
      return freeVars(ast.body);
    case kindForall:
    case kindExists:
      return new Set([ast.name, ...freeVars(ast.body)]);
    case kindName:
      return new Set([ast]);
    case kindPredicate:
      return new Set([ast.target].concat(ast.args));
    case kindDeclaration:
      var result = freeVars(ast.body);
      result.delete(ast.name);
    case kindInvalid:
    case kindEmpty:
    case kindBottom:
      return new Set();
    default:
      throw "forgot a case... " + ast.kind;
  }
}
const RARR = "&rarr;";
function justifyForallIntroduction(goal, scope, linenos) {
  if (goal.kind !== kindForall) {
    return null;
  }
  for (let i = 0; i < scope.length; i++) {
    let proof = scope[i];
    if (proof instanceof Proof
     && proof.assumption.kind === kindDeclaration
     && proof.assumption.body.kind === kindEmpty
     && varRepl(proof.conclusion, proof.assumption.name, goal.name).equals(goal.body)) {
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
      if (item instanceof Proof || item.kind !== kindForall) {
        continue;
      }
      if (varRepl(item.body, item.name, name).equals(goal)) {
        return FORALL + "E:" + linenos[i] + "[" + item.name.name + RARR + name.name + "]";
      }
    }
  }
}
function justifyExistsIntroduction(goal, scope, linenos) {
  console.log(goal, scope, linenos);
  if (goal.kind !== kindExists) {
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
      console.log(varRepl(item, name, goal.name), varRepl(item, name, goal.name).equals(goal.body));
      if (varRepl(item, name, goal.name).equals(goal.body)) {
        return EXISTS + "I:" + linenos[i] + "[" + name.name + RARR + goal.name.name + "]";
      }
    }
  }
}
function justifyExistsElimination(goal, scope, linenos) {
  for (let i = 0; i < scope.length; i++) {
    let iline = scope[i];
    if (iline instanceof Proof || iline.kind !== kindExists) {
      continue;
    }
    for (let j = 0; j < scope.length; j++) {
      let jproof = scope[j];
      if (jproof instanceof Proof
       && jproof.assumption.kind === kindDeclaration
       && jproof.conclusion.equals(goal)
       && iline.body.equals(varRepl(jproof.assumption.body, jproof.assumption.name, iline.name))) {
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
