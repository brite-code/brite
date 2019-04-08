module.exports = {
  preset: 'ts-jest',
  globals: {
    __DEBUG__: true,
    'ts-jest': {
      babelConfig: false,
      diagnostics: false,
    },
  },
};
