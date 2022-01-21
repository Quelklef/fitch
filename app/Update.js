
exports.setUrlArg = arg => () => {
  window.history.replaceState(null, '', window.location.pathname + "?proof=" + encodeURI(arg));
};

// ↓ Stolen from https://stackoverflow.com/a/45308151/4608364
const copyToClipboard_impl =
function(){const e=document.createElement("textarea");return e.style.cssText
="position: absolute; left: -99999em",e.setAttribute("readonly",!0),document.
body.appendChild(e),function(t){e.value=t;const n=document.getSelection().
rangeCount>0&&document.getSelection().getRangeAt(0);if(navigator.userAgent.
match(/ipad|ipod|iphone/i)){const t=e.contentEditable;e.contentEditable=!0;
const n=document.createRange();n.selectNodeContents(e);const o=window.
getSelection();o.removeAllRanges(),o.addRange(n),e.setSelectionRange(0,999999)
,e.contentEditable=t}else e.select();try{const e=document.execCommand("copy");
return n&&(document.getSelection().removeAllRanges(),document.getSelection()
.addRange(n)),e}catch(e){return console.error(e),!1}}}();

exports.copyToClipboard = str => () => copyToClipboard_impl(str);

exports.getByIdAndSetFocus = id => () => {
  const $el = document.getElementById(id);
  if (!$el) {
    console.warn('No element with id', id);
    return;
  }
  $el.focus();
}

// Set the value of an input, changing caret position only minimally
exports.reupInput = ({ id, value }) => () => {
  const $input = document.getElementById(id);
  if (!$input || !($input instanceof HTMLInputElement)) {
    console.error(`Unable to reup ${id}`);
    return;
  }

  const i = $input.selectionEnd + (value.length - $input.value.length);
  $input.value = value;
  $input.selectionStart = $input.selectionEnd = i;
};
