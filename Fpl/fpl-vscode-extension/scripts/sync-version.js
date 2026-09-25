// Syncs the "version" field in package.json with <Version> from the solution's Directory.Build.props,
// so that `vsce package` / `vsce publish` always ships a package.json matching the .NET solution version.

'use strict';

const fs = require('fs');
const path = require('path');

const extensionRoot = path.resolve(__dirname, '..');
const solutionDir = path.resolve(extensionRoot, '..');
const buildPropsPath = path.join(solutionDir, 'Directory.Build.props');
const packageJsonPath = path.join(extensionRoot, 'package.json');

function log(message) {
    console.log(`[sync-version] ${message}`);
}

function readVersionFromBuildProps(filePath) {
    if (!fs.existsSync(filePath)) {
        throw new Error(`Directory.Build.props not found at ${filePath}`);
    }

    const xml = fs.readFileSync(filePath, 'utf8');
    const match = xml.match(/<VersionPrefix>\s*([^<\s]+)\s*<\/VersionPrefix>/);

    if (!match) {
        throw new Error(`Could not find <VersionPrefix> in ${filePath}`);
    }

    return match[1];
}

function updatePackageJsonVersion(newVersion) {
    const packageJson = JSON.parse(fs.readFileSync(packageJsonPath, 'utf8'));

    if (packageJson.version === newVersion) {
        log(`package.json version already ${newVersion}, no update needed.`);
        return;
    }

    packageJson.version = newVersion;
    fs.writeFileSync(packageJsonPath, JSON.stringify(packageJson, null, 4) + '\n', 'utf8');
    log(`package.json version updated to ${newVersion}`);
}

function main() {
    const version = readVersionFromBuildProps(buildPropsPath);
    updatePackageJsonVersion(version);
}

main();
