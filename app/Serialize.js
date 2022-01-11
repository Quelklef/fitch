exports.toPayload = str => {
  try {
    return btoa(unescape(encodeURIComponent(str)));
  } catch (e) {
    console.warn(e);
    return '';
  }
};

exports.fromPayload = pay => {
  try {
    return decodeURIComponent(escape(atob(pay)));
  } catch (e) {
    console.warn(e);
    return '';
  }
};
