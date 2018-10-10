// tslint:disable no-any

import {
  Term,
  abstraction,
  application,
  binding,
  native,
  variable,
} from './term';
import {Visitor, visit} from './visitor';

let record: Array<[string, Term<any>]> = [];

const recordingVisitor: Visitor<any> = {
  enterVariable: term => {
    record.push(['enterVariable', term]);
  },
  leaveVariable: term => {
    record.push(['leaveVariable', term]);
  },
  enterAbstraction: term => {
    record.push(['enterAbstraction', term]);
  },
  leaveAbstraction: term => {
    record.push(['leaveAbstraction', term]);
  },
  enterApplication: term => {
    record.push(['enterApplication', term]);
  },
  leaveApplication: term => {
    record.push(['leaveApplication', term]);
  },
  enterNative: term => {
    record.push(['enterNative', term]);
  },
  leaveNative: term => {
    record.push(['leaveNative', term]);
  },
};

beforeEach(() => {
  record = [];
});

function unreachable() {
  throw new Error('Unreachable');
}

test('enters and leaves a variable', () => {
  const term = variable('x');
  expect(visit(term, recordingVisitor)).toBe(term);
  expect(record).toEqual([['enterVariable', term], ['leaveVariable', term]]);
});

test('enters and leaves an abstraction', () => {
  const term = abstraction('x', variable('x'));
  expect(visit(term, recordingVisitor)).toBe(term);
  expect(record).toEqual([
    ['enterAbstraction', term],
    ['enterVariable', term.body],
    ['leaveVariable', term.body],
    ['leaveAbstraction', term],
  ]);
});

test('enters and leaves an application', () => {
  const term = application(variable('x'), variable('y'));
  expect(visit(term, recordingVisitor)).toBe(term);
  expect(record).toEqual([
    ['enterApplication', term],
    ['enterVariable', term.callee],
    ['leaveVariable', term.callee],
    ['enterVariable', term.argument],
    ['leaveVariable', term.argument],
    ['leaveApplication', term],
  ]);
});

test('enters and leaves a binding', () => {
  const term: any = binding('x', variable('y'), variable('z'));
  expect(visit(term, recordingVisitor)).toBe(term);
  expect(record).toEqual([
    ['enterApplication', term],
    ['enterAbstraction', term.callee],
    ['enterVariable', variable('z')],
    ['leaveVariable', variable('z')],
    ['leaveAbstraction', term.callee],
    ['enterVariable', variable('y')],
    ['leaveVariable', variable('y')],
    ['leaveApplication', term],
  ]);
});

test('enters and leaves a native term with no inputs', () => {
  const term = native([], unreachable);
  expect(visit(term, recordingVisitor)).toBe(term);
  expect(record).toEqual([['enterNative', term], ['leaveNative', term]]);
});

test('enters and leaves a native term with one input', () => {
  const term = native([variable('x')], unreachable);
  expect(visit(term, recordingVisitor)).toBe(term);
  expect(record).toEqual([
    ['enterNative', term],
    ['enterVariable', variable('x')],
    ['leaveVariable', variable('x')],
    ['leaveNative', term],
  ]);
});

test('enters and leaves a native term with many inputs', () => {
  const term = native(
    [variable('a'), variable('b'), variable('c'), variable('d'), variable('e')],
    unreachable,
  );
  expect(visit(term, recordingVisitor)).toBe(term);
  expect(record).toEqual([
    ['enterNative', term],
    ['enterVariable', variable('a')],
    ['leaveVariable', variable('a')],
    ['enterVariable', variable('b')],
    ['leaveVariable', variable('b')],
    ['enterVariable', variable('c')],
    ['leaveVariable', variable('c')],
    ['enterVariable', variable('d')],
    ['leaveVariable', variable('d')],
    ['enterVariable', variable('e')],
    ['leaveVariable', variable('e')],
    ['leaveNative', term],
  ]);
});

test('skips when false is returned from an application enter function', () => {
  const term = application(variable('x'), variable('y'));
  expect(
    visit(term, {
      ...recordingVisitor,
      enterApplication: term => {
        recordingVisitor.enterApplication(term);
        return false;
      },
    }),
  ).toBe(term);
  expect(record).toEqual([['enterApplication', term]]);
});

test('skips when false is returned from an abstraction enter function', () => {
  const term: any = binding('x', variable('y'), variable('z'));
  expect(
    visit(term, {
      ...recordingVisitor,
      enterAbstraction: term => {
        recordingVisitor.enterAbstraction(term);
        return false;
      },
    }),
  ).toBe(term);
  expect(record).toEqual([
    ['enterApplication', term],
    ['enterAbstraction', term.callee],
    ['enterVariable', variable('y')],
    ['leaveVariable', variable('y')],
    ['leaveApplication', term],
  ]);
});

test('skips when false is returned from a variable enter function', () => {
  const term = application(variable('x'), variable('y'));
  expect(
    visit(term, {
      ...recordingVisitor,
      enterVariable: term => {
        recordingVisitor.enterVariable(term);
        return false;
      },
    }),
  ).toBe(term);
  expect(record).toEqual([
    ['enterApplication', term],
    ['enterVariable', term.callee],
    ['enterVariable', term.argument],
    ['leaveApplication', term],
  ]);
});

test('skips when false is returned conditionally from a variable enter function 1', () => {
  const term = application(variable('x'), variable('y'));
  expect(
    visit(term, {
      ...recordingVisitor,
      enterVariable: term => {
        recordingVisitor.enterVariable(term);
        return term.name === 'x' ? false : undefined;
      },
    }),
  ).toBe(term);
  expect(record).toEqual([
    ['enterApplication', term],
    ['enterVariable', term.callee],
    ['enterVariable', term.argument],
    ['leaveVariable', term.argument],
    ['leaveApplication', term],
  ]);
});

test('skips when false is returned conditionally from a variable enter function 2', () => {
  const term = application(variable('x'), variable('y'));
  expect(
    visit(term, {
      ...recordingVisitor,
      enterVariable: term => {
        recordingVisitor.enterVariable(term);
        return term.name === 'y' ? false : undefined;
      },
    }),
  ).toBe(term);
  expect(record).toEqual([
    ['enterApplication', term],
    ['enterVariable', term.callee],
    ['leaveVariable', term.callee],
    ['enterVariable', term.argument],
    ['leaveApplication', term],
  ]);
});

test('replaces when a term is returned from enter function 1', () => {
  const term = application(variable('a'), variable('b'));
  const newTerm: any = visit(term, {
    ...recordingVisitor,
    enterVariable: term => {
      recordingVisitor.enterVariable(term);
      if (term.name === 'a') return variable('x');
      return undefined;
    },
  });
  expect(newTerm).toEqual(application(variable('x'), variable('b')));
  expect(newTerm).not.toBe(term);
  expect(term.callee).not.toBe(newTerm.callee);
  expect(term.argument).toBe(newTerm.argument);
  expect(record).toEqual([
    ['enterApplication', term],
    ['enterVariable', variable('a')],
    ['enterVariable', variable('x')],
    ['leaveVariable', variable('x')],
    ['enterVariable', variable('b')],
    ['leaveVariable', variable('b')],
    ['leaveApplication', newTerm],
  ]);
});

test('replaces when a term is returned from enter function 2', () => {
  const term = application(variable('a'), variable('b'));
  const newTerm: any = visit(term, {
    ...recordingVisitor,
    enterVariable: term => {
      recordingVisitor.enterVariable(term);
      if (term.name === 'b') return variable('x');
      return undefined;
    },
  });
  expect(newTerm).toEqual(application(variable('a'), variable('x')));
  expect(newTerm).not.toBe(term);
  expect(term.callee).toBe(newTerm.callee);
  expect(term.argument).not.toBe(newTerm.argument);
  expect(record).toEqual([
    ['enterApplication', term],
    ['enterVariable', variable('a')],
    ['leaveVariable', variable('a')],
    ['enterVariable', variable('b')],
    ['enterVariable', variable('x')],
    ['leaveVariable', variable('x')],
    ['leaveApplication', newTerm],
  ]);
});

test('replaces when a term is returned from the leave function 1', () => {
  const term = application(variable('a'), variable('b'));
  const newTerm: any = visit(term, {
    ...recordingVisitor,
    leaveVariable: term => {
      recordingVisitor.leaveVariable(term);
      if (term.name === 'a') return variable('x');
      return undefined;
    },
  });
  expect(newTerm).toEqual(application(variable('x'), variable('b')));
  expect(newTerm).not.toBe(term);
  expect(term.callee).not.toBe(newTerm.callee);
  expect(term.argument).toBe(newTerm.argument);
  expect(record).toEqual([
    ['enterApplication', term],
    ['enterVariable', variable('a')],
    ['leaveVariable', variable('a')],
    ['enterVariable', variable('b')],
    ['leaveVariable', variable('b')],
    ['leaveApplication', newTerm],
  ]);
});

test('replaces when a term is returned from the leave function 2', () => {
  const term = application(variable('a'), variable('b'));
  const newTerm: any = visit(term, {
    ...recordingVisitor,
    leaveVariable: term => {
      recordingVisitor.leaveVariable(term);
      if (term.name === 'b') return variable('x');
      return undefined;
    },
  });
  expect(newTerm).toEqual(application(variable('a'), variable('x')));
  expect(newTerm).not.toBe(term);
  expect(term.callee).toBe(newTerm.callee);
  expect(term.argument).not.toBe(newTerm.argument);
  expect(record).toEqual([
    ['enterApplication', term],
    ['enterVariable', variable('a')],
    ['leaveVariable', variable('a')],
    ['enterVariable', variable('b')],
    ['leaveVariable', variable('b')],
    ['leaveApplication', newTerm],
  ]);
});
