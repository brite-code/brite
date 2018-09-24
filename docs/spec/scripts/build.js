const fs = require('fs');
const path = require('path');
const chalk = require('chalk').default;
const specMarkdown = require('spec-md');

const srcPath = path.join(process.cwd(), 'src', 'index.md');
const buildPath = path.join(process.cwd(), 'build', 'index.html');

async function build() {
  const html = await specMarkdown.html(srcPath);
  await mkdir(path.dirname(buildPath));
  await writeFile(buildPath, html);
  console.log(`Built: ${chalk.cyan.bold(buildPath)}`);
}

if (!module.parent) {
  build().catch(error => {
    setImmediate(() => {
      throw error;
    });
  });
}

module.exports = build;

/**
 * Makes a directory but does not fail if the directory already exists.
 *
 * @param {string} dirPath
 */
function mkdir(dirPath) {
  return new Promise((resolve, reject) => {
    fs.mkdir(dirPath, error => {
      if (error && error.code !== 'EEXIST') {
        reject(error);
      } else {
        resolve();
      }
    });
  });
}

/**
 * Writes a file and returns a promise.
 *
 * @param {string} filePath
 * @param {string} fileContents
 */
function writeFile(filePath, fileContents) {
  return new Promise((resolve, reject) => {
    fs.writeFile(filePath, fileContents, error => {
      if (error) {
        reject(error);
      } else {
        resolve();
      }
    });
  });
}
