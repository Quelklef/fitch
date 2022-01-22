
const isBrowser = typeof window !== 'undefined';

const toNumeral = function(num, base, toDigit) {
  const str = [];
  while (num > BigInt(0)) {
    const d = toDigit(num % base);
    str.push(d);
    num = num / base;
  }
  return str.reverse().join('');
}

const fromNumeral = function(digs, base, fromDigit) {
  let num = BigInt(0);
  for (let i = 0; i < digs.length; i++) {
    const d = fromDigit(digs[i]);
    num = num * base + d;
  }
  return num;
}

const indexOfEx = function(string, char) {
  const i = string.indexOf(char);
  if (i === -1) throw Error('Cannot find');
  return i;
}

const indexIntoEx = function(string, idx) {
  if (idx < 0 || idx >= string.length) throw Error('Bad index');
  return string[idx];
}


// Base-62 charset
const bchars = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
const b62ToInt = str => fromNumeral(str, BigInt(bchars.length), c => BigInt(indexOfEx(bchars, c)));
const intToB62 = num => toNumeral(num, BigInt(bchars.length), n => indexIntoEx(bchars, Number(n)));

// Reduced charset we might expect to encode, as per domain knowledge
const rsigchars = '¬∧∨→↔⊥⊤∀∃=≠()[]{}\\:; abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
const rzero = '%';  // Zeroes collapse on the left of a numeral; that is, rToInt(rzero + s) === rToInt(s)
                    // Don't want to lose information, so choose a zero char outside of our domain charset
                    // This is a hack, but it should work.
const rchars = rzero + rsigchars;
const rToInt = str => fromNumeral(str, BigInt(rchars.length), c => BigInt(indexOfEx(rchars, c)));
const intToR = num => toNumeral(num, BigInt(rchars.length), n => indexIntoEx(rchars, Number(n)));

// Unicode
const uniToInt = str => fromNumeral(str, BigInt(65536), c => BigInt(c.charCodeAt(0)));
const intToUni = num => toNumeral(num, BigInt(65536), n => {
  if (n < BigInt(0) || n >= BigInt(65536)) throw Error('Bad N');
  return String.fromCharCode(Number(n));
});

function encode(str) {
  const problemChars = new Set([...str].filter(c => !rsigchars.includes(c) || c === rzero));;
  if (problemChars.size === 0) {
    return 'r' + intToB62(rToInt(str));
  } else {
    if (isBrowser)
      console.info('Cannot use reduced encoding due to these chars:', [...problemChars].join(''));
    return 'u' + intToB62(uniToInt(str));
  }
}

function decode(str) {
  if (str.startsWith('r')) {
    return intToR(b62ToInt(str.slice(1)));
  } else if (str.startsWith('u')) {
    return intToUni(b62ToInt(str.slice(1)));
  } else {
    throw Error('Cannot decode');
  }
}

exports.toPayload = string => {
  try {
    return encode(string);
  } catch (e) {
    console.warn('Failed to encode', e);
    return '';
  }
};

exports.fromPayload_f = string => {
  if (!string) return null;
  try {
    return decode(string);
  } catch (e) {
    console.warn('Failed to decode', e);
    return null;
  }
};
