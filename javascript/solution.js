const a = [];
const b = [];

for (let i = 10; i < 100; i++) {
  a.push(i);
  b.push(i);
}

const digitCodesSrc = {
  '0': '1111110',
  '1': '0110000',
  '2': '1101101',
  '3': '1111001',
  '4': '0110011',
  '5': '1011011',
  '6': '1011111',
  '7': '1110000',
  '8': '1111111',
  '9': '1111011',
};

const digitCodes = Object.fromEntries(
  Object.entries(digitCodesSrc).map(([key, val]) => [
    key, val.split('').map((x) => Number(x)),
  ]),
);

const result = [];

for (const a1 in a) {
  for (const b1 in b) {
    if (a1 * b1 >= 100 && a1 * b1 < 10000) {
      result.push([a1, b1, a1 * b1]);
    }
  }
}

const zip = (arr1, arr2) => arr1.map((x, i) => [x, arr2[i]]);
const padLeft = (s, ord) => {
  const padCount = ord - s.length;
  const padStr = new Array(padCount).fill(0);
  return `${padStr}${s}`;
}

const countDigitDistance = (x, y) => (
  zip(digitCodes[x], digitCodes[y])
    .reduce((acc, [x1, y1]) => {
      const diff = y1 - x1;
      if (diff === -1) return [acc[0] + 1, acc[1]];
      if (diff === 1) return [acc[0], acc[1] + 1];
      return acc;
    }, [0, 0])
);

const countDistance = (s1, s2) => (
  zip(s1.split(''), s2.split(''))
    .reduce(
      (acc, x) => {
        const d = countDigitDistance(x[0], x[1]);
        const res = [acc[0] + d[0], acc[1] + d[1]];
        return res;
      },
      [0, 0],
    )
);

const srcNum = '25691725';

const final = result.map(([a1, b1, c1]) => [a1, b1, c1.toString()])
  .map(([a1, b1, c1]) => [padLeft(a1, 2), padLeft(b1, 2), padLeft(c1, 4)])
  .filter(([a1, b1, r]) => {
    const t = `${a1}${b1}${r}`;
    const diff = countDistance(srcNum, t);
    return diff[0] === 5 && diff[1] === 5;
  });

console.log(final);
console.log(final.length);
