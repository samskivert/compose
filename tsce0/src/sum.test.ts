it('adds 1 + 2 to equal 3 in Typescript', () => {
  const sum = require('./sum.ts');
  expect(sum(1, 2)).toBe(3);
});

it('moar addition', () => {
  const sum = require('./sum.ts');
  expect(sum(41, 1)).toBe(42);
});
