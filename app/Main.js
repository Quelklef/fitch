exports.getUrlArg = () => {
  const url = window.location.href
  const key = '?proof=';
  const idx = url.indexOf(key)
  const encoded = idx === -1 ? "" : url.slice(idx + key.length);
  const decoded = decodeURI(encoded);
  console.log("Url argument:", decoded);
  return decoded;
}
