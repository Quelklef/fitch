
const charset = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';

function encode(s) {
  let n = 0n;

  for (let i = 0; i < s.length; i++)
    n = n * 65536n + BigInt(s.charCodeAt(i));

  const cs = [];
  const b = BigInt(charset.length);
  while (n > BigInt(0)) {
    cs.push(charset[n % b]);
    n = n / b;
  }
  return cs.reverse().join('');
}

function decode(s) {
  let n = 0n;

  for (let i = 0; i < s.length; i++) {
    const d = charset.indexOf(s[i]);
    if (d === -1) {
      console.warn('cannot decode');
      return '';
    }
    n = n * BigInt(charset.length) + BigInt(d);
  }

  const cs = [];
  const b = 65536n;
  while (n > BigInt(0)) {
    cs.push(String.fromCharCode(Number(n % b)));
    n = n / b;
  }
  return cs.reverse().join('');
}


exports.toPayload = encode;
exports.fromPayload = decode;
