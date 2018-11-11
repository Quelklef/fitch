'use strict';

// requires: main*js

function displayExample(ex) {
  proof = ex;
  show();
  focusAt([0]);
}

$('#prop-dm-or').click(() => {
  displayExample(
    new Proof([
      parse(""),
      new Proof([
        parse("~(P+Q)"),
        new Proof([
          parse("P"),
          parse("P+Q"),
          parse("(P+Q)*~(P+Q)"),
          parse("#"),
        ]),
        parse("~P"),
        new Proof([
          parse("Q"),
          parse("P+Q"),
          parse("(P+Q)*~(P+Q)"),
          parse("#"),
        ]),
        parse("~Q"),
        parse("~P*~Q"),
      ]),
      new Proof([
        parse("~P*~Q"),
        new Proof([
          parse("P+Q"),
          new Proof([
            parse("P"),
            parse("~P"),
            parse("P*~P"),
            parse("#"),
          ]),
          new Proof([
            parse("Q"),
            parse("~Q"),
            parse("Q*~Q"),
            parse("#"),
          ]),
          parse("#"),
        ]),
        parse("~(P+Q)")
      ]),
      parse("~(P+Q)=(~P*~Q)"),
    ]),
  );
});

$('#prop-dm-and').click(() => {
  displayExample(
    new Proof([
      parse(""),
      new Proof([
        parse("~(P*Q)"),
        new Proof([
          parse("~(~P+~Q)"),
          new Proof([
            parse("~P"),
            parse("~P+~Q"),
            parse("(~P+~Q)*~(~P+~Q)"),
            parse("#"),
          ]),
          parse("~~P"),
          parse("P"),
          new Proof([
            parse("~Q"),
            parse("~P+~Q"),
            parse("(~P+~Q)*~(~P+~Q)"),
            parse("#"),
          ]),
          parse("~~Q"),
          parse("Q"),
          parse("P*Q"),
          parse("(P*Q)*~(P*Q)"),
          parse("#"),
        ]),
        parse("~~(~P+~Q)"),
        parse("~P+~Q"),
      ]),
      new Proof([
        parse("~P+~Q"),
        new Proof([
          parse("P*Q"),
          new Proof([
            parse("~P"),
            parse("P"),
            parse("P*~P"),
            parse("#"),
          ]),
          new Proof([
            parse("~Q"),
            parse("Q"),
            parse("Q*~Q"),
            parse("#"),
          ]),
          parse("#"),
        ]),
        parse("~(P*Q)"),
      ]),
      parse("~(P*Q)=(~P+~Q)"),
    ]),
  );
});

$('#fol-dm-exist').click(() => {
  displayExample(
    new Proof([
      parse(""),
      new Proof([
        parse("~ExP(x)"),
        new Proof([
          parse("[a]"),
          new Proof([
            parse("P(a)"),
            parse("ExP(x)"),
            parse("(ExP(x))*~(ExP(x))"),
            parse("#"),
          ]),
          parse("~P(a)"),
        ]),
        parse("Vx~P(x)"),
      ]),
      new Proof([
        parse("Vx~P(x)"),
        new Proof([
          parse("ExP(x)"),
          new Proof([
            parse("[a]P(a)"),
            parse("~P(a)"),
            parse("P(a)*~P(a)"),
            parse("#"),
          ]),
          parse("#"),
        ]),
        parse("~ExP(x)"),
      ]),
      parse("(~ExP(x))=(Vx~P(x))"),
    ]),
  );
});

$('#fol-dm-forall').click(() => {
  displayExample(
    new Proof([
      parse(""),
      new Proof([
        parse("~VxP(x)"),
        new Proof([
          parse("~Ex~P(x)"),
          new Proof([
            parse("[a]"),
            new Proof([
              parse("~P(a)"),
              parse("Ex~P(x)"),
              parse("(Ex~P(x))*~(Ex~P(x))"),
              parse("#"),
            ]),
            parse("~~P(a)"),
            parse("P(a)"),
          ]),
          parse("VxP(x)"),
          parse("(VxP(x))*~(VxP(x))"),
          parse("#")
        ]),
        parse("~~Ex~P(x)"),
        parse("Ex~P(x)"),
      ]),
      new Proof([
        parse("Ex~P(x)"),
        new Proof([
          parse("VxP(x)"),
          new Proof([
            parse("[a]~P(a)"),
            parse("P(a)"),
            parse("P(a)*~P(a)"),
            parse("#"),
          ]),
          parse("#"),
        ]),
        parse("~VxP(x)"),
      ]),
      parse("(~VxP(x))=(Ex~P(x))"),
    ]),
  );
});

function toProof(lines) {
  if (lines instanceof Array) return new Proof(lines.map(toProof));
  return parse(lines);
}
function simpleExample(selector, lines) {
  $(selector).click(() => displayExample(toProof(lines)));
}

simpleExample('#reiteration', ['P', 'P']);
simpleExample('#conjunctionI', ['P', ['Q', 'P*Q']]);
simpleExample('#conjunctionE1', ['P*Q', 'P']);
simpleExample('#conjunctionE2', ['P*Q', 'Q']);
simpleExample('#disjunctionI', ['P', 'P+Q']);
simpleExample('#disjunctionE', ['P+(P*Q)', ['P', 'P'], ['P*Q', 'P'], 'P']);
simpleExample('#implicationI', ['Q', ['P', 'Q'], 'P>Q']);
simpleExample('#implicationE', ['P>Q', ['P', 'Q']]);
simpleExample('#biconditionalI', ['P*Q', ['P', 'Q'], ['Q', 'P'], 'P=Q']);
simpleExample('#biconditionalE1', ['P=Q', ['P', 'Q']]);
simpleExample('#biconditionalE2', ['P=Q', ['Q', 'P']]);
simpleExample('#bottomI', ['P*~P', '#']);
simpleExample('#negationI', ['~(P+Q)', ['P', 'P+Q', '(P+Q)*~(P+Q)', '#'], '~P']);
simpleExample('#negationE', ['~~P', 'P']);
simpleExample(
  '#forallI',
  [ ''
  , [ '[y]'
    , [ '~(P(y)+~P(y))'
      , [ 'P(y)'
        , 'P(y)+~P(y)'
        , '(P(y)+~P(y))*~(P(y)+~P(y))'
        , '#'
      ]
      , '~P(y)'
      , 'P(y)+~P(y)'
      , '(P(y)+~P(y))*~(P(y)+~P(y))'
      , '#'
    ]
    , '~~(P(y)+~P(y))'
    , 'P(y)+~P(y)'
  ]
  , 'VxP(x)+~P(x)'
]
);
simpleExample('#forallE', ['VxP(x)', ['[y]', 'P(y)']]);
simpleExample('#existsI', ['', ['[x]P(x)', 'EyP(y)']]);
simpleExample('#existsE', ['ExP(x)', ['[y]P(y)', 'P(y)+Q(y)', 'EyP(y)+Q(y)'], 'EyP(y)+Q(y)']);
