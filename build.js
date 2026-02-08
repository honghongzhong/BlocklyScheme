const fs = require('fs');
const path = require('path');

const distDir = path.join(__dirname, 'dist');
const libDir = path.join(distDir, 'lib');

console.log('Building to ./dist...');

// 1. Clean dist
if (fs.existsSync(distDir)) {
  fs.rmSync(distDir, { recursive: true, force: true });
}
fs.mkdirSync(libDir, { recursive: true });

// 2. Copy App Files
const appFiles = [
  'index.html',
  'styles.css',
  'app.js',
  'blocks.js',
  'generator.js',
  'i18n.js'
];

appFiles.forEach(file => {
  const srcPath = path.join(__dirname, file);
  if (fs.existsSync(srcPath)) {
    fs.copyFileSync(srcPath, path.join(distDir, file));
  } else {
    console.warn(`Warning: ${file} not found`);
  }
});

// 3. Copy Dependencies

// --- Blockly ---
const blocklySrc = path.join(__dirname, 'node_modules', 'blockly');
const blocklyDist = path.join(libDir, 'blockly');
fs.mkdirSync(blocklyDist, { recursive: true });

// Core files
['blockly_compressed.js', 'blocks_compressed.js'].forEach(f => {
  fs.copyFileSync(path.join(blocklySrc, f), path.join(blocklyDist, f));
});

// Msg (en.js)
const msgDestDir = path.join(blocklyDist, 'msg');
fs.mkdirSync(msgDestDir, { recursive: true });
fs.copyFileSync(
  path.join(blocklySrc, 'msg', 'en.js'),
  path.join(msgDestDir, 'en.js')
);

// Media
const mediaSrc = path.join(blocklySrc, 'media');
const mediaDest = path.join(blocklyDist, 'media');
if (fs.existsSync(mediaSrc)) {
  fs.cpSync(mediaSrc, mediaDest, { recursive: true });
}

// --- BiwaScheme ---
// Path: node_modules/biwascheme/release/biwascheme-min.js
const biwaDestDir = path.join(libDir, 'biwascheme');
fs.mkdirSync(biwaDestDir, { recursive: true });
fs.copyFileSync(
  path.join(__dirname, 'node_modules', 'biwascheme', 'release', 'biwascheme-min.js'),
  path.join(biwaDestDir, 'biwascheme-min.js')
);


// 4. Update index.html paths
const indexHtmlPath = path.join(distDir, 'index.html');
let html = fs.readFileSync(indexHtmlPath, 'utf8');

// Replace: node_modules/blockly/ -> lib/blockly/
html = html.replace(/node_modules\/blockly\//g, 'lib/blockly/');

// Replace: node_modules/biwascheme/release/ -> lib/biwascheme/
html = html.replace(/node_modules\/biwascheme\/release\//g, 'lib/biwascheme/');

fs.writeFileSync(indexHtmlPath, html);

console.log('Build complete! Deploy the contents of the "dist" folder.');
