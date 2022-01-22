
// Good luck trying to read this :V
exports.parseProof_f = ({ mkLine, mkBlock }) => string => {

  string = string.split('\n').map(l => l.trim()).filter(l => !!l).join('\n');
  return impl(string);

  function impl(string) {

    const lines = string.split('\n');

    const barIdx = lines.findIndex(ln => ln.startsWith('├─'));
    const headLines = lines.slice(0, barIdx).map(ln => ln.slice(1));
    const bodyLines = lines.slice(barIdx + 1).map(ln => ln.slice(1));

    const head = headLines.map(getContent);

    const body = [];
    let lo = 0;
    for (let i = 0; i < bodyLines.length + 1; i++) {
      const bodyLine = bodyLines[i];

      if (!bodyLine || bodyLine.startsWith(' ')) {
        const block = bodyLines.slice(lo, i).join('\n');
        if (block) body.push(impl(block));
        lo = i + 1;
      }

      if (bodyLine && bodyLine.startsWith(' ')) {
        const content = getContent(bodyLine);
        if (content) body.push(mkLine(content));
      }

    }

    return mkBlock(head)(body);

  }

  function getContent(str) {
    const i = str.indexOf('. ') + 2;
    let j = str.indexOf('  ', i);
    if (j === -1) j = str.length;
    return str.slice(i, j);
  };

};


exports.trim4 = str => str.trim().split('\n').map(l => l.trim()).join('\n');


exports.linewiseDiff_f = ({ mkTup }) => s1 => s2 => {
  const lines1 = s1.split('\n');
  const lines2 = s2.split('\n');

  const result = [];
  for (let i = 0; i < Math.max(lines1.length, lines2.length); i++) {
    const l1 = lines1[i];
    const l2 = lines2[i];
    if (l1 !== l2) result.push(mkTup(l1)(l2));
  }
  return result;
};
