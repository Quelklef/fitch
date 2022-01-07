exports.getKeyCodeShiftKey = ev =>
  ({ keyCode: ev.keyCode
   , shiftKey: ev.shiftKey });

exports.doPreventDefault = ev => () =>
  ev.preventDefault();
