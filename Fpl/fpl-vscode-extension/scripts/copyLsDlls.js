// Refreshes the Fpl Lanaguage Server (FplLS) release build and copies its output into out/FplLsDll
// so that `vsce package` / `vsce publish` always ships an up-to-date language server.

'use strict';

const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

const extensionRoot = path.resolve(__dirname, '..');
const solutionDir = path.resolve(extensionRoot, '..');
const dllSourceDir = path.join(solutionDir, 'FplLS', 'bin', 'Release', 'net8.0');
const dllTargetDir = path.join(extensionRoot, 'out', 'FplLsDll');

function log(message) {
    console.log(`[copyLsDlls] ${message}`);
}

function removeDirRecursive(dir) {
    if (fs.existsSync(dir)) {
        log(`Removing ${dir}`);
        fs.rmSync(dir, { recursive: true, force: true });
    }
}

function copyFilteredRecursive(srcDir, destDir) {
    log(`Copying *.dll / *.json from ${srcDir} to ${destDir}`);
    fs.mkdirSync(destDir, { recursive: true });
    fs.cpSync(srcDir, destDir, {
        recursive: true,
        filter: (src) => {
            const stat = fs.existsSync(src) ? fs.statSync(src) : null;
            if (stat && stat.isDirectory()) {
                return true;
            }
            const ext = path.extname(src).toLowerCase();
            return ext === '.dll' || ext === '.json';
        }
    });
}

function publishSolution() {
    log('Running "dotnet clean -c Release" ...');
    execSync('dotnet clean -c Release', {
        cwd: solutionDir,
        stdio: 'inherit'
    });

    // avoid caching (For a release/publish pipeline where correctness matters more than speed), we want to force a clean build once in a while.
    log('Running "dotnet publish -c Release" ...');
    execSync('dotnet publish -c Release', {
        cwd: solutionDir,
        stdio: 'inherit'
    });
}

function main() {
    removeDirRecursive(dllTargetDir);
    publishSolution();
    copyFilteredRecursive(dllSourceDir, dllTargetDir);
    log('Done.');
}

main();