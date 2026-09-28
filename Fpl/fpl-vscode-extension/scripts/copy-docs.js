// Copies solution-wide documentation files into the VS Code extension folder,
// so that `vsce package` / `vsce publish` always ships up-to-date,
// extension-specific copies of these files.

'use strict';

const fs = require('fs');
const path = require('path');

const extensionRoot = path.resolve(__dirname, '..');
const solutionDir = path.resolve(extensionRoot, '..');
const repoRoot = path.resolve(solutionDir, '..');

// Each entry maps a source file (relative to its base directory) to a target
// file (relative to the extension root).
const filesToCopy = [
    {
        source: path.join(repoRoot, 'README.md'),
        target: path.join(extensionRoot, 'README.md')
    },
    {
        source: path.join(repoRoot, 'LICENSE'),
        target: path.join(extensionRoot, 'LICENSE')
    }
];

function log(message) {
    console.log(`[copy-release-notes] ${message}`);
}

function copyFile(source, target) {
    if (!fs.existsSync(source)) {
        throw new Error(`Source file not found at ${source}`);
    }
    log(`Copying ${source} to ${target}`);
    fs.copyFileSync(source, target);
}

function main() {
    for (const { source, target } of filesToCopy) {
        copyFile(source, target);
    }
    log('Done.');
}

main();
