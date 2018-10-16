import {typeVariableCounter, typeVariableName} from './prefix';

test('typeVariableName encodes numbers', () => {
  expect(typeVariableName(0)).toEqual('a');
  expect(typeVariableName(1)).toEqual('b');
  expect(typeVariableName(2)).toEqual('c');
  expect(typeVariableName(25)).toEqual('z');
  expect(typeVariableName(26)).toEqual('t1');
  expect(typeVariableName(100)).toEqual('t75');
});

test('typeVariableCounter decodes strings', () => {
  expect(typeVariableCounter('a')).toEqual(0);
  expect(typeVariableCounter('b')).toEqual(1);
  expect(typeVariableCounter('c')).toEqual(2);
  expect(typeVariableCounter('z')).toEqual(25);
  expect(typeVariableCounter('t1')).toEqual(26);
  expect(typeVariableCounter('t75')).toEqual(100);
});

test('typeVariableCounter does not decode ill-formatted strings', () => {
  expect(typeVariableCounter('A')).toEqual(undefined);
  expect(typeVariableCounter('ts')).toEqual(undefined);
  expect(typeVariableCounter('t0t')).toEqual(undefined);
  expect(typeVariableCounter('t1.1')).toEqual(undefined);
  expect(typeVariableCounter('t0')).toEqual(undefined);
  expect(typeVariableCounter('t01')).toEqual(undefined);
});
