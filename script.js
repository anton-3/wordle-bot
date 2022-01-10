function getSolution() {
  const a = localStorage.gameState.split('solution')[1];
  return a.slice(a.indexOf(':') + 2, a.indexOf(',') - 1);
}