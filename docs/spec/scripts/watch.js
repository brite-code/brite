const path = require('path');
const browserSync = require('browser-sync').create();
const chokidar = require('chokidar');
const build = require('./build');

const buildPath = path.join(process.cwd(), 'build', 'index.html');
const srcWatchPath = path.join(process.cwd(), 'src', '**', '*.md');

rebuild().then(() => {
  browserSync.init({server: path.dirname(buildPath)});

  const srcWatcher = chokidar.watch(srcWatchPath, {ignoreInitial: true});

  srcWatcher
    .on('add', rebuild)
    .on('change', rebuild)
    .on('unlink', rebuild);

  const buildWatcher = chokidar.watch(buildPath, {ignoreInitial: true});

  buildWatcher.on('change', () => browserSync.reload('index.html'));
});

async function rebuild() {
  try {
    await build();
  } catch (error) {
    console.error(error.stack ? error.stack : error.message);
  }
}
