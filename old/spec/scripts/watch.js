let path = require('path');
let browserSync = require('browser-sync').create();
let chokidar = require('chokidar');
let build = require('./build');

let buildPath = path.join(process.cwd(), 'build', 'index.html');
let srcWatchPath = path.join(process.cwd(), 'src', '**', '*.md');

rebuild().then(() => {
  browserSync.init({server: path.dirname(buildPath)});

  let srcWatcher = chokidar.watch(srcWatchPath, {ignoreInitial: true});

  srcWatcher
    .on('add', rebuild)
    .on('change', rebuild)
    .on('unlink', rebuild);

  let buildWatcher = chokidar.watch(buildPath, {ignoreInitial: true});

  buildWatcher.on('change', () => browserSync.reload('index.html'));
});

async function rebuild() {
  try {
    await build();
  } catch (error) {
    console.error(error.stack ? error.stack : error.message);
  }
}
