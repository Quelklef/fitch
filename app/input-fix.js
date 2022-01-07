
/*

As part of the proof helper, we prettify what the user types into the lines.
If they type, for instance,

  Pa.-Qa.~Ra

this will be prettified to

  Pa&~Qa&~Ra

if unicode is disabled or

  Pa∧¬Qa∧¬Ra

if unicode is enabled.

This process involves getting the value of the user input element, prettifying
that gotten text, and then setting the value to the prettified text.

Unfortunately, if the prettified text differs from the original text, the caret
will be set to the end of the input.

This file contains code that managers the cursor to counteract this behaviour.

*/

// Applies the fix to a single input node.
// Idempotent; repeated calls to this with the same node will have no
// effect beyond the first call.
function fixInput(inputNode) {

  // Ensure idempotency
  if (inputNode.__alreadyFixed) return;
  inputNode.__alreadyFixed = true;

  // Keep track of the node value
  let prevValue = inputNode.value;
  // Keep track of the caret position
  // In the case of a range, use the end of the range. It "just works" this way =)
  let prevOffset = inputNode.selectionEnd;

  inputNode.addEventListener('input', event => {
    const newValue = inputNode.value

    // Set the selection back to where it "should be"
    // Accounts for the case that multiple chars are collapsed (e.g. /=) into one (≠)!
    const i = prevOffset + (newValue.length - prevValue.length);
    setTimeout(() => inputNode.selectionStart = inputNode.selectionEnd = i, 0);

    prevValue = newValue;
    prevOffset = inputNode.selectionEnd;
  });

  inputNode.addEventListener('keydown', event => {
    prevOffset = inputNode.selectionEnd;
  });

  inputNode.addEventListener('keyup', event => {
    prevOffset = inputNode.selectionEnd;
  });

}

// ↓↓↓↓↓↓
// Watch the app for mutations and apply the fix to new
// inputs as they are created
// (Because this script is run before the app is initialized,
//  there is no need to check for existing inputs)

const observer = new MutationObserver(mutations => {
  for (const mutation of mutations) {
    for (const addedNode of [...mutation.addedNodes]) {
      if (addedNode instanceof Element) {
        const inputs = addedNode.querySelectorAll('.line\\:input');
        [...inputs].forEach(fixInput);
      }
    }
  }
})

observer.observe(
  document.body,
  {
    subtree: true,  // recursively observe child nodes
    childList: true,  // listen for child node additions/deletions
  }
);
