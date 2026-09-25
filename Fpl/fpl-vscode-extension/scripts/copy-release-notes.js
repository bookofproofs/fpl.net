// Copies the solution-wide docs/RELEASE_NOTES.md into the VS Code extension's
// RELEASE_NOTES.md, so that `vsce package` / `vsce publish` always ships an
// up-to-date, extension-specific copy of the release notes.

'use strict';

const fs = require('fs');
const path = require('path');

const extensionRoot = path.resolve(__dirname, '..');
const solutionDir = path.resolve(extensionRoot, '..');
const sourceReleaseNotesPath = path.join(solutionDir, 'docs', 'RELEASE_NOTES.md');
const targetReleaseNotesPath = path.join(extensionRoot, 'RELEASE_NOTES.md');

function log(message) {
    console.log(`[copy-release-notes] ${message}`);
}

function main() {
    if (!fs.existsSync(sourceReleaseNotesPath)) {
        throw new Error(`Source release notes not found at ${sourceReleaseNotesPath}`);
    }
    log(`Copying ${sourceReleaseNotesPath} to ${targetReleaseNotesPath}`);
    fs.copyFileSync(sourceReleaseNotesPath, targetReleaseNotesPath);
    log('Done.');
}

main();
