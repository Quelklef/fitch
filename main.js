'use strict';

/*
POSSIBLE FEATURES:
- Rewrite & code qual. first!!
- Highlight selected line
- Highlight direct dependencies of selected line
  (i.e. the lines referenced in its justification)
- Highlight lines that depend on an invalid line,
  or lines that depend on one of these lines
- Change negation into from
    . A , -A
    . _
  to
    . A
  . -A
  . _
*/

const IMPLICATION = "&rarr;";
const BICONDITIONAL = "&harr;";
const NEGATION = "&not;";
const CONJUNCTION = "&and;";
const DISJUNCTION = "&or;";
const OPEN = "(";
const CLOSE = ")";
const BOTTOM = "&perp;";

function prettify(string, minify=true) {
  var result = "";
  for (let i = 0; i < string.length; i++) {
    let char = string[i];
    if (/^[a-z]$/i.test(char)) {
      result += char.toUpperCase();
    } else {
      result += {
        ">": IMPLICATION,
        "=": BICONDITIONAL,
        "-": NEGATION,
        ",": CONJUNCTION,
        "|": DISJUNCTION,
        "(": OPEN,
        ")": CLOSE,
        "_": BOTTOM
      }[char] || (minify ? "" : char);
    }
  }
  return result;
}

function parseParens(string) {
  if (string.startsWith(OPEN)) {
    let [result, rest] = parse(string.slice(OPEN.length));
    if (!rest.startsWith(CLOSE)) {
      throw "Bad syntax";
    }
    return [result, rest.slice(CLOSE.length)];
  }
}

function parseAtom(string) {
  if (string.startsWith(OPEN)) { return parseParens(string); }
  
  if (string === "") {
    throw "Expected atom";
  }

  if (string.startsWith(BOTTOM)) {
    return [{
      kind: "bottom"
    }, string.slice(BOTTOM.length)];
  }
  
  return [{
    kind: "variable",
    name: string[0]
  }, string.slice(1)];
}

function parseNegation(string) {
  if (string.startsWith(NEGATION)) {
    let [body, rest] = parseNegation(string.slice(NEGATION.length));
    return [{
      kind: "negation",
      body: body
    }, rest];
  }
  return parseAtom(string);
}

function parseDisjunction(string) {
  let [lhs, rest] = parseNegation(string);
  if (rest.startsWith(DISJUNCTION)) {
    let [rhs, restt] = parseNegation(rest.slice(DISJUNCTION.length));
    return [{
      kind: "disjunction",
      lhs: lhs,
      rhs: rhs
    }, restt];
  }
  return [lhs, rest];
}

function parseConjunction(string) {
  let [lhs, rest] = parseDisjunction(string);
  if (rest.startsWith(CONJUNCTION)) {
    let [rhs, restt] = parseDisjunction(rest.slice(CONJUNCTION.length));
    return [{
      kind: "conjunction",
      lhs: lhs,
      rhs: rhs
    }, restt];
  }
  return [lhs, rest];
}

function parseImplication(string) {
  let [lhs, rest] = parseConjunction(string);
  if (rest.startsWith(IMPLICATION)) {
    let [rhs, restt] = parseConjunction(rest.slice(IMPLICATION.length));
    return [{
      kind: "implication",
      lhs: lhs,
      rhs: rhs
    }, restt];
  }
  return [lhs, rest];
}

function parseBiconditional(string) {
  let [lhs, rest] = parseImplication(string);
  if (rest.startsWith(BICONDITIONAL)) {
    let [rhs, restt] = parseImplication(rest.slice(BICONDITIONAL.length));
    return [{
      kind: "biconditional",
      lhs: lhs,
      rhs: rhs
    }, restt];
  }
  return [lhs, rest];
}

function parse(string) {
  return parseBiconditional(string);
}

function line(lineno) {
  this.parent = null;
  this.lineno = lineno;
  this.overlay = $("<span>", {class: "overlay"});
  this.input = $("<input>", {class: "input"});
  this.proof = $("<p>", {class: "proof"});
  
  this.parsed = {kind: "invalid"};
  
  // When the user is editing an input, all keypresses go to the input directly and cannot
  // be caught by event handlers on `document`. So, we add a handler to each input box which
  // calls the global keypress handler.
  $(this.input).on('input', e => {
    return keypress(e, this);
  });
  $(this.input).on('keydown', e => {
    // Because .on('input', f) doesn't capture all keys
    if (["Enter", "Tab", "ArrowUp", "ArrowDown", "Backspace"].includes(e.key)) {
      return keypress(e, this);
    }
    return true;
  });
  
  // Local keypress handler
  this.keypress = e => {
    this.show();

    var parsed, rest;
    try {
      [parsed, rest] = parse(prettify(this.input.val()));
    } catch (e) {
      parsed = {kind: "invalid"};
    }
    if (rest !== "") {
      parsed = {kind: "invalid"};
    }
    this.parsed = parsed;

    proveFrom(this);
    return false;
  };
  
  this.linedom = $("<span>", {class: "lineno"});
  this.dom = $("<p>", {class: "line"}).append(
    this.linedom.html(this.lineno),
    $("<span>", {class: "input-group"}).append(
      this.input,
      this.overlay,
    ),
    this.proof
  );

  this.show = () => {
    this.overlay.html(prettify(this.input.val(), false));
  };
  
  this.root = () => {
    let v = this;
    while (v.parent !== null) {
      v = v.parent;
    }
    return v;
  }

  return this;
}

function context() {
  this.parent = null;
  this.dom = $("<div>", {
    class: "context"
  });
  this.children = [];
  this.push = (item) => {
    item.parent = this;
    this.children.push(item);
    this.dom.append(item.dom);
  };
  this.insert = (i, item) => {
    item.parent = this;
    this.children.splice(i, 0, item);
    this.dom.children().eq(i - 1).after(item.dom);
  };
  this.replace = (i, item) => {
    item.parent = this;
    this.children[i] = item;
    this.dom.children().eq(i - 1).after(item.dom);
  };
  this.pop = () => {
    this.dom.children()[this.dom.children().length - 1].remove();
    return this.children.pop();
  };
  this.removeAt = (i) => {
    let item = this.children[i];
    this.children.splice(i, 1);
    item.dom.remove();
  };
  this.show = () => { };
  this.lastLine = () => {
    // Recursively find the last line
    let last = this.children[this.children.length - 1];
    if (last instanceof context) {
      return last.lastLine();
    } else { // last instanceof line
      return last;
    }
  };
  this.lines = () => {
    return this.children[0].lineno + "-" + this.children[this.children.length - 1].lineno;
  };
  
  return this;
}


let root = new context();
let firstLine = new line(1);
root.push(firstLine);
$("#proof-root").append(root.dom);
firstLine.input.focus();

function findLine(lineno, rootCtx = root) {
  for (let i = 0; i < rootCtx.children.length; i++) {
    let item = rootCtx.children[i];
    if (item instanceof line) {
      if (item.lineno == lineno) {
        return item;
      }
    } else { // item instanceof context
      let res = findLine(lineno, item);
      if (res) {
        return res;
      }
    }
  }
}

function flash(el) {
  el.removeClass("flash");
  // A tad bit hacky
  setTimeout(() => el.addClass("flash"), 15);
}

function keypress(e, item) {
  // Note that `item instanceof line`
  if (e.key === "Backspace") {
    if (item.input.val() === "") {
      backspace(item);
      return false;
    }
    return true;
  } else if (e.key === "Enter" && !e.shiftKey && !e.ctrlKey) {
    let newLine = new line(item.lineno + 1);
    item.parent.insert(item.parent.children.indexOf(item) + 1, newLine);
    newLine.input.focus();
    renumber(root, 1);
    proveFrom(newLine);
    return false;
  } else if (e.key === "Tab" && !e.shiftKey) {
    let i = item.parent.children.indexOf(item);
    
    // Don't allow on first line (assumption line)
    if (i === 0) {
      flash($("#new-assumption-restriction"));
      return false;
    }
    
    var ctx = new context();
    item.parent.replace(i, ctx);
    ctx.push(item);
    item.input.focus();
    
    proveFrom(item);
    return false;
  } else if (e.key === "Tab" && e.shiftKey) {
    // Don't unindent first line
    if (item.lineno == 1) {
      return false;
    }
    
    // Don't unindent something at 0 indentation
    if (!item.parent.parent) {
      return false;
    }
    
    let i = item.parent.children.indexOf(item);
    
    // Only allow shift-tab on last line of context
    if (i !== item.parent.children.length - 1) {
      flash($("#end-assumption-restriction"))
      return false;
    }
    
    let wasOnlyLineInContext = item.parent.children.length == 1;
    let parentI = item.parent.parent.children.indexOf(item.parent);
    
    item.parent.pop();
    if (wasOnlyLineInContext) {
      item.parent.parent.removeAt(parentI);
      item.parent.parent.insert(parentI, item);
    } else {
      item.parent.parent.insert(item.parent.parent.children.indexOf(item.parent) + 1, item);
    }
    item.input.focus();
    
    proveFrom(item);
    return false;
  } else if (e.key === "ArrowUp") {
    (findLine(item.lineno - 1) || item).input.focus();
  } else if (e.key === "ArrowDown") {
    (findLine(item.lineno + 1) || item).input.focus();
  }
  return item.keypress(e);
}

function backspace(someLine) {
  // Do nothing if is the only line
  if (someLine.lineno === 1 && !findLine(2)) { return; }
  
  someLine.parent.removeAt(someLine.parent.children.indexOf(someLine));
  if (someLine.parent.children.filter(c => c instanceof line).length !== 0) {
    someLine.parent.lastLine().input.focus();
  } else {
    let grandparent = someLine.parent.parent;
    someLine.parent.parent.removeAt(someLine.parent.parent.children.indexOf(someLine.parent));
  }
  
  renumber(root, 1);
  (findLine(someLine.lineno - 1) || findLine(1)).input.focus();
  
  let l = findLine(someLine.lineno);
  if (l) {
    proveFrom(l);
  }
}

function renumber(context, startN) {
  var lineno = startN;
  for (let item of context.children) {
    if (item instanceof line) {
      item.lineno = lineno;
      item.linedom.html(lineno);
      lineno += 1;
    } else { // context
      lineno = renumber(item, lineno);
    }
  }
  return lineno;
}

function scope(someLine) {
  var result = [];
  var ctx = someLine.parent;
  while (ctx) {
    for (let i = 0; i < ctx.children.length; i++) {
      let item = ctx.children[i];
      if (item instanceof line && item.lineno >= someLine.lineno
         || item instanceof context && item.lastLine().lineno >= someLine.lineno) {
        break;
      }
      // TODO: Make no null line; only {kind: "invalid"}
      if (item instanceof context || item && item.parsed) { // Ignore null (empty proposition)
        result.push(item);
      }
    }
    ctx = ctx.parent;
  }
  return result;
}

function reiteration(someLine) {
  for (let scoped of scope(someLine)) {
    if (scoped instanceof line && _.isEqual(scoped.parsed, someLine.parsed)) {
      return "R:" + scoped.lineno;
    }
  }
}

function bottomIntroduction(someLine) {
  if (someLine.parsed.kind !== "bottom") { return; }
  for (let scoped of scope(someLine)) {
    if (scoped instanceof line
       && scoped.parsed.kind == "conjunction"
       && (scoped.parsed.lhs.kind == "negation" && _.isEqual(scoped.parsed.lhs.body, scoped.parsed.rhs)
         || scoped.parsed.rhs.kind == "negation" && _.isEqual(scoped.parsed.lhs, scoped.parsed.rhs.body))) {
      return BOTTOM + "I:" + scoped.lineno;
    }
  }
}

function negationIntroduction(someLine) {
  if (someLine.parsed.kind !== "negation") { return; }
  for (let scoped of scope(someLine)) {
    if (scoped instanceof context) {
      let last = scoped.children[scoped.children.length - 1];
      let first = scoped.children[0];
      if (last instanceof line
        && first instanceof line
          && last.parsed.kind === "bottom"
          && _.isEqual(first.parsed, someLine.parsed.body)) {
        return NEGATION + "I:" + scoped.lines();
      }
    }
  }
}

function negationElimination(someLine) {
  for (let scoped of scope(someLine)) {
    if (scoped instanceof line
       && scoped.parsed.kind == "negation"
       && scoped.parsed.body.kind == "negation"
       && _.isEqual(scoped.parsed.body.body, someLine.parsed)) {
      return NEGATION + "E:" + scoped.lineno;
    }
  }
}

function conjunctionIntroduction(someLine) {
  if (someLine.parsed.kind !== "conjunction") { return; }
  var lhsProof, rhsProof;
  for (let scoped of scope(someLine)) {
    if (scoped instanceof line
       && _.isEqual(scoped.parsed, someLine.parsed.lhs)) {
      lhsProof = scoped;
      break;
    }
  }
  for (let scoped of scope(someLine)) {
    if (scoped instanceof line
       && _.isEqual(scoped.parsed, someLine.parsed.rhs)) {
      rhsProof = scoped;
      break;
    }
  }
  if (!(lhsProof && rhsProof)) { return null; }
  return CONJUNCTION + "I:" + lhsProof.lineno + "," + rhsProof.lineno;
}

function conjunctionElimination(someLine) {
  for (let scoped of scope(someLine)) {
    if (scoped instanceof line
       && scoped.parsed.kind == "conjunction"
       && (_.isEqual(scoped.parsed.lhs, someLine.parsed)
         || _.isEqual(scoped.parsed.rhs, someLine.parsed))) {
      return CONJUNCTION + "E:" + scoped.lineno;
    }
  }
}

function disjunctionIntroduction(someLine) {
  if (someLine.parsed.kind !== "disjunction") { return; }
  for (let scoped of scope(someLine)) {
    if (scoped instanceof line
       && (_.isEqual(someLine.parsed.lhs, scoped.parsed)
         || _.isEqual(someLine.parsed.rhs, scoped.parsed))) {
      return DISJUNCTION + "I:" + scoped.lineno;
    }
  }
}

function disjunctionElimination(someLine) {
  for (let disj of scope(someLine).filter(l => l instanceof line
                      && l.parsed.kind === "disjunction")) {
    let ctxsWithOneSideOfDisj =
      scope(someLine).filter(c => c instanceof context
                   && (_.isEqual(c.children[0].parsed, disj.parsed.lhs)
                     || _.isEqual(c.children[0].parsed, disj.parsed.rhs))
                   && (_.isEqual(c.children[c.children.length - 1].parsed, someLine.parsed)) // Make sure the conclusion is what we want
                   );
    
    for (let ctxA of ctxsWithOneSideOfDisj) {
      for (let ctxB of ctxsWithOneSideOfDisj) {
        if ((_.isEqual(disj.parsed.lhs, ctxA.children[0].parsed) && _.isEqual(disj.parsed.rhs, ctxB.children[0].parsed))
           || (_.isEqual(disj.parsed.lhs, ctxB.children[0].parsed) && _.isEqual(disj.parsed.rhs, ctxA.children[0].parsed))) {
          return DISJUNCTION + "E:" + disj.lineno + "," + ctxA.lines() + "," + ctxB.lines();
        }
      }
    }
  }
}

function implicationIntroduction(someLine) {
  if (someLine.parsed.kind !== "implication") { return; }
  for (let scoped of scope(someLine)) {
    if (scoped instanceof context
       && _.isEqual(scoped.children[0].parsed, someLine.parsed.lhs)
       && _.isEqual(scoped.children[scoped.children.length - 1].parsed, someLine.parsed.rhs)) {
      return IMPLICATION + "I:" + scoped.lines();
    }
  }
}

function implicationElimination(someLine) {
  let implications = scope(someLine).filter(l => l instanceof line && l.parsed.kind == "implication" && _.isEqual(l.parsed.rhs, someLine.parsed));
  for (let implication of implications) {
    for (let scoped of scope(someLine)) {
      if (_.isEqual(scoped.parsed, implication.parsed.lhs)) {
        return IMPLICATION + "E:" + implication.lineno + "," + scoped.lineno;
      }
    }
  }
}

function biconditionalIntroduction(someLine) {
  if (someLine.parsed.kind !== "biconditional") { return; }
  var toProof, fromProof;
  for (let scoped of scope(someLine).filter(c => c instanceof context)) {
    if (_.isEqual(scoped.children[0].parsed, someLine.parsed.lhs)
       && _.isEqual(scoped.children[scoped.children.length - 1].parsed, someLine.parsed.rhs)) {
      toProof = scoped;
    }
    if (_.isEqual(scoped.children[scoped.children.length - 1].parsed, someLine.parsed.lhs)
       && _.isEqual(scoped.children[0].parsed, someLine.parsed.rhs)) {
      fromProof = scoped;
    }
    if (toProof && fromProof) {
      return BICONDITIONAL + "I:" + toProof.lines() + "," + fromProof.lines();
    }
  }
}

function biconditionalElimination(someLine) {
  for (let bicon of scope(someLine).filter(l => l instanceof line && l.parsed.kind == "biconditional")) {
    if (!(_.isEqual(someLine.parsed, bicon.parsed.lhs)
       || _.isEqual(someLine.parsed, bicon.parsed.rhs))) {
      continue;
    }
    for (let scoped of scope(someLine)) {
      if (_.isEqual(scoped.parsed, bicon.parsed.lhs)
         && _.isEqual(bicon.parsed.rhs, someLine.parsed)
         || _.isEqual(scoped.parsed, bicon.parsed.rhs)
         && _.isEqual(bicon.parsed.lhs, someLine.parsed)) {
        return BICONDITIONAL + "E:" + bicon.lineno + "," + scoped.lineno;
      }
    }
  }
}

const proofStrategies = [
  reiteration,
  bottomIntroduction,
  negationIntroduction, negationElimination,
  conjunctionIntroduction, conjunctionElimination,
  disjunctionIntroduction, disjunctionElimination,
  implicationIntroduction, implicationElimination,
  biconditionalIntroduction, biconditionalElimination
]

function proof(someLine) {
  if (someLine.parent.children.indexOf(someLine) === 0) {
    // If assumption
    return "assumption";
  }
   
  for (let i = 0; i < proofStrategies.length; i++) {
    let strat = proofStrategies[i];
    let proof = strat(someLine);
    if (proof) {
      return proof;
    }
  }
}

function prove(someLine) {
  if (someLine instanceof context) {
    // TODO: Not sure if necessary
    someLine.children.forEach(prove);
    return;
  }
  
  someLine.dom.removeClass("invalid").removeClass("parse-error");
  
  if (!someLine.input.val()) {
    someLine.proof.html("");
    return;
  }
  
  if (someLine.parsed.kind === "invalid") {
    someLine.dom.addClass("parse-error");
    someLine.proof.html("?");
    return;
  }
  
  let p = proof(someLine);
  if (p) {
    someLine.proof.html(p);
  } else {
    someLine.dom.addClass("invalid");
    someLine.proof.html("!");
  }
}

function proveFrom(someLine) {
  // Prove a line and all lines after it
  if (!someLine.parent) return;
  let i = someLine.parent.children.indexOf(someLine);
  for (let j = i; j < someLine.parent.children.length; j++) {
    prove(someLine.parent.children[j]);
  }
  proveFrom(someLine.parent);
}
