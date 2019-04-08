let fs = require('fs');
let path = require('path');
let chalk = require('chalk').default;
let specMarkdown = require('spec-md');

let srcPath = path.join(process.cwd(), 'src', 'index.md');
let buildPath = path.join(process.cwd(), 'build', 'index.html');

async function build() {
  let html = await specMarkdown.html(srcPath);
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
