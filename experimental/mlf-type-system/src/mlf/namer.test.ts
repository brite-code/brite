import {counterFromName, nameFromCounter} from './namer';

test('nameFromCounter encodes numbers', () => {
  expect(nameFromCounter(0)).toEqual('a');
  expect(nameFromCounter(1)).toEqual('b');
  expect(nameFromCounter(2)).toEqual('c');
  expect(nameFromCounter(25)).toEqual('z');
  expect(nameFromCounter(26)).toEqual('t1');
  expect(nameFromCounter(100)).toEqual('t75');
});

test('counterFromName decodes strings', () => {
  expect(counterFromName('a')).toEqual(0);
  expect(counterFromName('b')).toEqual(1);
  expect(counterFromName('c')).toEqual(2);
  expect(counterFromName('z')).toEqual(25);
  expect(counterFromName('t1')).toEqual(26);
  expect(counterFromName('t75')).toEqual(100);
});

test('counterFromName does not decode ill-formatted strings', () => {
  expect(counterFromName('A')).toEqual(undefined);
  expect(counterFromName('ts')).toEqual(undefined);
  expect(counterFromName('t0t')).toEqual(undefined);
  expect(counterFromName('t1.1')).toEqual(undefined);
  expect(counterFromName('t0')).toEqual(undefined);
  expect(counterFromName('t01')).toEqual(undefined);
});
