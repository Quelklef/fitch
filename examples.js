'use strict';

// requires: main.js

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
    ]),
  );
});

$('#prop-dm-and').click(() => {
  displayExample(
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
    ]),
  );
});

$('#fol-dm-exist').click(() => {
  displayExample(
    new Proof([
      parse(""),
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
    ]),
  );
});

$('#fol-dm-forall').click(() => {
  displayExample(
    new Proof([
      parse(""),
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
simpleExample('#conjunctionI', ['P', ['Q', 'P.Q']]);
simpleExample('#conjunctionE1', ['P.Q', 'P']);
simpleExample('#conjunctionE2', ['P.Q', 'Q']);
simpleExample('#disjunctionI', ['P', 'P|Q']);
simpleExample('#disjunctionE', ['P|(P.Q)', ['P', 'P'], ['P.Q', 'P'], 'P']);
simpleExample('#implicationI', ['Q', ['P', 'Q'], 'P>Q']);
simpleExample('#implicationE', ['P>Q', ['P', 'Q']]);
simpleExample('#biconditionalI', ['P.Q', ['P', 'Q'], ['Q', 'P'], 'P=Q']);
simpleExample('#biconditionalE1', ['P=Q', ['P', 'Q']]);
simpleExample('#biconditionalE2', ['P=Q', ['Q', 'P']]);
simpleExample('#bottomI', ['P.-P', '_']);
simpleExample('#negationI', ['-(P|Q)', ['P', 'P|Q', '(P|Q).-(P|Q)', '_'], '-P']);
simpleExample('#negationE', ['--P', 'P']);
simpleExample(
  '#forallI',
  [ ''
  , [ '[y]'
    , [ '-(P(y)|-P(y))'
      , [ 'P(y)'
        , 'P(y)|-P(y)'
        , '(P(y)|-P(y)).-(P(y)|-P(y))'
        , '_'
      ]
      , '-P(y)'
      , 'P(y)|-P(y)'
      , '(P(y)|-P(y)).-(P(y)|-P(y))'
      , '_'
    ]
    , '--(P(y)|-P(y))'
    , 'P(y)|-P(y)'
  ]
  , 'VxP(x)|-P(x)'
]
);
simpleExample('#forallE', ['VxP(x)', ['[y]', 'P(y)']]);
simpleExample('#existsI', ['P(x)', 'EyP(y)']);
simpleExample('#existsE', ['ExP(x)', ['[y]P(y)', 'P(y)|Q(y)', 'EyP(y)|Q(y)'], 'EyP(y)|Q(y)']);
