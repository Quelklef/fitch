'use strict';

// requires: parse.js

/* Contains the algorithms for justifying propositions.
   Each algorithm takes 3 arguments: `line`, `scope`, and `linenos`.
   `line` is the AST Node of the proposition.
   `scope` is a list of other propositions as well as proofs in the 'scope"
   of the line, i.e., able to be used as proof.
   `linenos` is a list, parallel to `scope`, that contains the line numbers
   of lines and starting line numbers of proofs.

    Also contains semantic checking for and distinguishing of
    propositonal variables, predicate variables, and name variables.

    Note that we allow combination of propositional and predicate logic
    by treating foralls and exists similar to propositional variables.
    We do NOT allow quantification over predicates. */

function justifyReiteration(line, scope, linenos) {
  for (let i = 0; i < scope.length; i++) {
    if (scope[i] instanceof Proposition && scope[i].concurs(line)) {
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
    if (!(line instanceof Proposition)) {
      continue;
    }
    if (line.concurs(goal.lhs)) {
      lhsPf = i;
    }
    if (line.concurs(goal.rhs)) {
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
    if (line instanceof Proposition && line.kind === kindConjunction && (line.lhs.concurs(goal) || line.rhs.concurs(goal))) {
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
    if (goal.lhs.concurs(line) || goal.rhs.concurs(line)) {
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
      if (!(jproof instanceof Proof) || !jproof.conclusion.concurs(goal)) {
        continue;
      }
      for (let k = 0; k < scope.length; k++) {
        let kproof = scope[k];
        if (!(kproof instanceof Proof) || !kproof.conclusion.concurs(goal)) {
          continue;
        }
        if (line.lhs.concurs(jproof.assumption) && line.rhs.concurs(kproof.assumption)
         || line.rhs.concurs(jproof.assumption) && line.lhs.concurs(kproof.assumption)) {
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
     && item.assumption.concurs(goal.lhs)
     && item.conclusion.concurs(goal.rhs)) {
      return IMPLICATION + "I:" + linenos[i] + "-" + (linenos[i+1]-1);
    }
  }
}
function justifyImplicationElimination(goal, scope, linenos) {
  for (let i = 0; i < scope.length; i++) {
    let iitem = scope[i];
    if (iitem instanceof Proof || iitem.kind !== kindImplication || !iitem.rhs.concurs(goal)) {
      continue;
    }
    for (let j = 0; j < scope.length; j++) {
      let jitem = scope[j];
      if (jitem instanceof Proof) {
        continue;
      }
      if (iitem.lhs.concurs(jitem)) {
        return IMPLICATION + "E:" + linenos[i] + "," + linenos[j];
      }
    }
  }
}
function justifyBiconditionalIntroducton(goal, scope, linenos) {
  if (goal.kind !== kindBiconditional) {
    return null;
  }
  function proofProposition(item) { return item instanceof Proof
                                    && (item.assumption.concurs(goal.lhs) || item.conclusion.concurs(goal.lhs))
                                    && (item.assumption.concurs(goal.rhs) || item.conclusion.concurs(goal.rhs)) };
  for (let i = 0; i < scope.length; i++) {
    let iproof = scope[i];
    if (!proofProposition(iproof)) {
      continue;
    }
    for (let j = 0; j < scope.length; j++) {
      let jproof = scope[j];
      if (!proofProposition(jproof)) {
        continue;
      }
      if (  (iproof.assumption.concurs(goal.lhs) && iproof.conclusion.concurs(goal.rhs)
          && jproof.assumption.concurs(goal.rhs) && jproof.conclusion.concurs(goal.lhs))
         || (iproof.assumption.concurs(goal.rhs) && iproof.conclusion.concurs(goal.lhs)
          && jproof.assumption.concurs(goal.lhs) && jproof.conclusion.concurs(goal.rhs)) ) {
        return BICONDITIONAL + "I:" + linenos[i] + "-" + (linenos[i+1]-1) + "," + linenos[j] + "-" + (linenos[j+1]-1);
      }
    }
  }
}
function justifyBiconditionalElimination(goal, scope, linenos) {
  for (let i = 0; i < scope.length; i++) {
    let item = scope[i];
    if (item instanceof Proposition && item.kind === kindBiconditional && (item.lhs.concurs(goal) || item.rhs.concurs(goal))) {
      return BICONDITIONAL + "E:" + linenos[i];
    }
  }
}
function justifyBottomIntroduction(goal, scope, linenos) {
  for (let i = 0; i < scope.length; i++) {
    let item = scope[i];
    if (item instanceof Proposition && item.kind === kindConjunction &&
        ((item.lhs.kind === kindNegation && item.lhs.body.concurs(item.rhs))
      || (item.rhs.kind === kindNegation && item.rhs.body.concurs(item.lhs)))) {
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
    if (item instanceof Proof && item.assumption.concurs(goal.body) && item.conclusion.kind === kindBottom) {
      return NEGATION + "I:" + linenos[i] + "-" + (linenos[i+1]-1);
    }
  }
}
function justifyNegationElimination(goal, scope, linenos) {
  for (let i = 0; i < scope.length; i++) {
    let item = scope[i];
    if (item instanceof Proposition && item.kind === kindNegation && item.body.kind === kindNegation && item.body.body.concurs(goal)) {
      return NEGATION + "E:" + linenos[i];
    }
  }
}

Proposition.prototype.substitute = function(nameFrom, nameTo) {
  /* Return `this` with the name `nameFrom` recursively replaced with `nameTo`.
     Only replaces items that can be quantified over in FOL (i.e., predicate arguments)
     And proposition variables; does not replace predicate names.
     Note that declarations are treated specially. */
  switch(this.kind) {
    case kindConjunction:
    case kindDisjunction:
    case kindImplication:
    case kindBiconditional:
      return Object.assign(new Proposition(this), {lhs: this.lhs.substitute(nameFrom, nameTo), rhs: this.rhs.substitute(nameFrom, nameTo)});
    case kindNegation:
      return Object.assign(new Proposition(this), {body: this.body.substitute(nameFrom, nameTo)});
    case kindForall:
    case kindExists:
      return Object.assign(new Proposition(this), {body: this.body.substitute(nameFrom, nameTo)});
    case kindName:
      return this.concurs(nameFrom) ? nameTo : this;
    case kindPredicate:
      // Do NOT replace the target since predicates cannot be quantified over in FOL
      return Object.assign(new Proposition(this), {args: this.args.map(arg => arg.substitute(nameFrom, nameTo))});
    case kindInvalid:
    case kindEmpty:
    case kindBottom:
      return this;
    default:
      throw "Missed case: " + this.kind;
  }
}
Proposition.prototype.freeNameVars = function(declFree, excluding = new Set()) {
  /* Recursively collect and return all free name variables.
     Excludes name nodes in the set `excluding`.
     Declarations should be considered to be free in some contexts but not in
     others; swap this with the `declFree` flag.
     For example, if declarations are considered to be free, then `x` is free
     in `[x]P(x)`. If not, then it isn't. */
  if (!declFree && this.declaring) {
    excluding = new Set([this.declaring, ...excluding]);
  }
  switch(this.kind) {
    case kindConjunction:
    case kindDisjunction:
    case kindImplication:
    case kindBiconditional:
      return new Set([...this.lhs.freeNameVars(declFree, excluding), ...this.rhs.freeNameVars(declFree, excluding)]);
    case kindNegation:
      return this.body.freeNameVars(declFree, excluding);
    case kindForall:
    case kindExists:
      return this.body.freeNameVars(declFree, new Set([this.name, ...excluding]));
    case kindName:
      return new Set();
    case kindPredicate:
      // Only return not-excluded arguments
      let ex = Array.from(excluding);
      return new Set(this.args.filter(name => !ex.some(prop => prop.concurs(name))));
    case kindInvalid:
    case kindEmpty:
    case kindBottom:
      return new Set();
    default:
      throw "forgot a case... " + this.kind;
  }
}
const RARR = String.fromCodePoint(0x2192);
function justifyForallIntroduction(goal, scope, linenos) {
  if (goal.kind !== kindForall) {
    return null;
  }
  for (let i = 0; i < scope.length; i++) {
    let proof = scope[i];
    if (proof instanceof Proof
     && proof.assumption.declaring
     && proof.assumption.kind === kindEmpty
     && proof.conclusion.substitute(proof.assumption.declaring, goal.name).concurs(goal.body)) {
      return FORALL + "I:" + linenos[i] + "-" + (linenos[i+1]-1) + "[" + proof.assumption.declaring.name + RARR + goal.name.name + "]";
    }
  }
}
function justifyForallElimination(goal, scope, linenos) {
  let names = Array.from(goal.freeNameVars(true));
  for (let n = 0; n < names.length; n++) {
    let name = names[n];
    for (let i = 0; i < scope.length; i++) {
      let item = scope[i];
      if (item instanceof Proof || item.kind !== kindForall) {
        continue;
      }
      if (item.body.substitute(item.name, name).concurs(goal)) {
        return FORALL + "E:" + linenos[i] + "[" + item.name.name + RARR + name.name + "]";
      }
    }
  }
}
function justifyExistsIntroduction(goal, scope, linenos) {
  if (goal.kind !== kindExists) {
    return null;
  }
  for (let i = 0; i < scope.length; i++) {
    let item = scope[i];
    if (item instanceof Proof) {
      continue;
    }
    let names = Array.from(item.freeNameVars(true));
    for (let n = 0; n < names.length; n++) {
      let name = names[n];
      if (item.substitute(name, goal.name).concurs(goal.body)) {
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
       && jproof.assumption.declaring
       && jproof.conclusion.concurs(goal)
       && iline.body.concurs(jproof.assumption.substitute(jproof.assumption.declaring, iline.name))) {
        return EXISTS + "E:" + linenos[i] + "," + linenos[j] + "-" + (linenos[j+1]-1);
      }
    }
  }
}
function justifyDomainNonEmpty(goal, scope, linenos) {
  for (let i = 0; i < scope.length; i++) {
    let item = scope[i];
    if (item instanceof Proof
     && item.assumption.declaring
     && item.assumption.kind === kindEmpty
     && item.conclusion.concurs(goal)) {
      return "D:" + linenos[i] + "-" + (linenos[i+1]-1);
    }
  }
}

function ensureNameVarsDeclared(prop, scope) {
  /* Ensure that all name variables are declared.
     If not, throw an error. */
  let names = Array.from(prop.freeNameVars(false));
  for (let n = 0; n < names.length; n++) {
    let name = names[n];
    if (!scope.some(line => line instanceof Proposition && name.concurs(line.declaring))) {
      throw `'${name.name}' is free`;
    }
  }
}

function ensureNotQuantifyingOverPropositions(prop, capturedNames = new Set()) {
  switch(prop.kind) {
    case kindConjunction:
    case kindDisjunction:
    case kindImplication:
    case kindBiconditional:
      ensureNotQuantifyingOverPropositions(prop.lhs, capturedNames);
      ensureNotQuantifyingOverPropositions(prop.rhs, capturedNames);
      break;
    case kindNegation:
      ensureNotQuantifyingOverPropositions(prop.body, capturedNames);
      break;
    case kindForall:
    case kindExists:
      ensureNotQuantifyingOverPropositions(prop.body, new Set([...capturedNames, prop.name]));
      break;
    case kindName:
      if (Array.from(capturedNames).some(name => name.concurs(prop))) {
        throw `Cannot quantify over proposition '${prop.name}'`;
      }
      break;
    case kindPredicate:
      break; // A predicate propositions consists of a predicate and name variables, so not propositons
    case kindInvalid:
    case kindEmpty:
    case kindBottom:
      break;
    default:
      throw "Missed case: " + prop.kind;
  }
}

function justify(line, scope, linenos, i) {
  /* Justify a line (AST Node) with all the lines of the given scope.
     The line numbers must be supplied in the parallel array `linenos`.
     `i` is the index of the line in its context.
     If the line cannot be justified, throws an error with the reason. */
   if (line.kind === kindInvalid) {
     throw line.error;
   }
   if (line.kind === kindEmpty) {
     return "";
   }

   ensureNameVarsDeclared(line, scope);
   ensureNotQuantifyingOverPropositions(line);

   if (i === 0) {
     return "assumption";
   } else {
     if (line.declaring) {
       throw "declaration must be assumption";
     }
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
    , justifyDomainNonEmpty
    ];

  for (let i = 0; i < strategies.length; i++) {
    let strat = strategies[i];
    var justification;
    try {
      justification = strat(line, scope, linenos);
    } catch (e) {
      console.log("justification error", strat);
      console.log(e);
      throw "program error";
    }
    if (justification) {
      return justification;
    }
  }

  throw "invalid step";
}
