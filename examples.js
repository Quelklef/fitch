'use strict';

// requires: main*js

function runTests() {
  let $exs = $('.example');
  function nextTest(i) {
    $($exs[i]).click();  // run example
    if (i < $exs.length) {
      setTimeout(() => nextTest(i+1), 100);
    }
  }
  nextTest(0);
}

// alt+T to run tests
$(document).keydown(ev => {
  if (ev.key === "t" && ev.altKey) {
    runTests();
  }
});

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
      parse("~(P+Q)/(~P*~Q)"),
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
      parse("~(P*Q)/(~P+~Q)"),
    ]),
  );
});

$('#fol-dm-exist').click(() => {
  displayExample(
    new Proof([
      parse(""),
      new Proof([
        parse("~ExPx"),
        new Proof([
          parse("[a]"),
          new Proof([
            parse("Pa"),
            parse("ExPx"),
            parse("(ExPx)*~(ExPx)"),
            parse("#"),
          ]),
          parse("~Pa"),
        ]),
        parse("Vx~Px"),
      ]),
      new Proof([
        parse("Vx~Px"),
        new Proof([
          parse("ExPx"),
          new Proof([
            parse("[a]Pa"),
            parse("~Pa"),
            parse("Pa*~Pa"),
            parse("#"),
          ]),
          parse("#"),
        ]),
        parse("~ExPx"),
      ]),
      parse("(~ExPx)/(Vx~Px)"),
    ]),
  );
});

$('#fol-dm-forall').click(() => {
  displayExample(
    new Proof([
      parse(""),
      new Proof([
        parse("~VxPx"),
        new Proof([
          parse("~Ex~Px"),
          new Proof([
            parse("[a]"),
            new Proof([
              parse("~Pa"),
              parse("Ex~Px"),
              parse("(Ex~Px)*~(Ex~Px)"),
              parse("#"),
            ]),
            parse("~~Pa"),
            parse("Pa"),
          ]),
          parse("VxPx"),
          parse("(VxPx)*~(VxPx)"),
          parse("#")
        ]),
        parse("~~Ex~Px"),
        parse("Ex~Px"),
      ]),
      new Proof([
        parse("Ex~Px"),
        new Proof([
          parse("VxPx"),
          new Proof([
            parse("[a]~Pa"),
            parse("Pa"),
            parse("Pa*~Pa"),
            parse("#"),
          ]),
          parse("#"),
        ]),
        parse("~VxPx"),
      ]),
      parse("(~VxPx)/(Ex~Px)"),
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
simpleExample('#biconditionalI', ['P*Q', ['P', 'Q'], ['Q', 'P'], 'P/Q']);
simpleExample('#biconditionalE1', ['P/Q', ['P', 'Q']]);
simpleExample('#biconditionalE2', ['P/Q', ['Q', 'P']]);
simpleExample('#bottomI', ['P*~P', '#']);
simpleExample('#negationI', ['~(P+Q)', ['P', 'P+Q', '(P+Q)*~(P+Q)', '#'], '~P']);
simpleExample('#negationE', ['~~P', 'P']);
simpleExample(
  '#forallI',
  [ ''
  , [ '[y]'
    , [ '~(Py+~Py)'
      , [ 'Py'
        , 'Py+~Py'
        , '(Py+~Py)*~(Py+~Py)'
        , '#'
      ]
      , '~Py'
      , 'Py+~Py'
      , '(Py+~Py)*~(Py+~Py)'
      , '#'
    ]
    , '~~(Py+~Py)'
    , 'Py+~Py'
  ]
  , 'VxPx+~Px'
]
);
simpleExample('#forallE', ['VxPx', ['[y]', 'Py']]);
simpleExample('#existsI', ['', ['[x]Px', 'EyPy']]);
simpleExample('#existsE', ['ExPx', ['[y]Py', 'Py+Qy', 'EyPy+Qy'], 'EyPy+Qy']);
simpleExample(
  '#nonempty',
  [ ''
  , [ '[x]'
    , [ '~(P+~P)'
      , [ 'P'
        , 'P+~P'
        , '(P+~P)*~(P+~P)'
        , '#'
      ]
      , '~P'
      , [ '~P'
        , 'P+~P'
        , '(P+~P)*~(P+~P)'
        , '#'
      ]
      , '~~P'
      , 'P'
      , '~P'
      , 'P*~P'
      , '#'
    ]
    , '~~(P+~P)'
    , 'P+~P'
  ]
  , 'P+~P'
]);
simpleExample('#equalsI', ['[x]', 'x=x']);
simpleExample('#equalsE', ['[x]', ['[y]', ['x=y*Px', 'x=y', 'Px', 'Py']]]);
