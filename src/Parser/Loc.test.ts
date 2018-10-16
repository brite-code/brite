import {Loc, Pos} from './Loc';

test('Pos.compare less than', () => {
  expect(new Pos(1, 1).compare(new Pos(2, 1))).toEqual(-1);
  expect(new Pos(1, 1).compare(new Pos(1, 2))).toEqual(-1);
});

test('Pos.compare greater than', () => {
  expect(new Pos(2, 1).compare(new Pos(1, 1))).toEqual(1);
  expect(new Pos(1, 2).compare(new Pos(1, 1))).toEqual(1);
});

test('Pos.compare equal to', () => {
  expect(new Pos(1, 1).compare(new Pos(1, 1))).toEqual(0);
  expect(new Pos(2, 1).compare(new Pos(2, 1))).toEqual(0);
  expect(new Pos(1, 2).compare(new Pos(1, 2))).toEqual(0);
  expect(new Pos(2, 2).compare(new Pos(2, 2))).toEqual(0);
});

test('Loc throws if start is greater than end', () => {
  expect(() => {
    new Loc(new Pos(2, 1), new Pos(1, 1)); // tslint:disable-line no-unused-expression
  }).toThrow();
});

test('Loc does not throw if start is less than end', () => {
  new Loc(new Pos(1, 1), new Pos(2, 1)); // tslint:disable-line no-unused-expression
});

test('Loc does not throw if start is equal to end', () => {
  new Loc(new Pos(1, 1), new Pos(1, 1)); // tslint:disable-line no-unused-expression
});
