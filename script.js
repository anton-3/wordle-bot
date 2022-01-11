function wrangle() {
  function getSolution() {
    const a = localStorage.gameState.split('solution')[1];
    return a.slice(a.indexOf(':') + 2, a.indexOf(',') - 1);
  }

  const keyboardElement = document.querySelector('game-app').$keyboard.$keyboard.children;
  const keyElements = {};

  for (let i = 0; i < keyboardElement.length; i++) {
    const row = keyboardElement[i].children;
    for (let j = 0; j < row.length; j++) {
      const key = row[j];
      const attr = key.getAttribute('data-key'); // will be the letter of the key e.g. 'q' if it is a key, null otherwise
      if (attr) keyElements[attr] = key;
    }
  }

  while (true) {
    const inp = prompt('click which letter?');
    keyElements[inp].click();
  }
}
