// Verifies that the newest released version in the root CHANGELOG.md matches the version
// defined in Directory.Build.props (VersionPrefix). Throws and aborts the publish process
// if they differ, since that indicates either the changelog or the version was forgotten to be bumped.

'use strict';

const fs = require('fs');
const path = require('path');

const extensionRoot = path.resolve(__dirname, '..');
const solutionDir = path.resolve(extensionRoot, '..');
const repoRoot = path.resolve(solutionDir, '..');

const buildPropsPath = path.join(solutionDir, 'Directory.Build.props');
const rootChangelogPath = path.join(repoRoot, 'CHANGELOG.md');

const VERSION_HEADER_REGEX = /^## \[v?([0-9]+\.[0-9]+\.[0-9]+(?:-[0-9A-Za-z.-]+)?)\]/;

function log(message) {
    console.log(`[check-version-changelog-match] ${message}`);
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

function readNewestVersionFromChangelog(filePath) {
    if (!fs.existsSync(filePath)) {
        throw new Error(`Root CHANGELOG.md not found at ${filePath}`);
    }

    const lines = fs.readFileSync(filePath, 'utf8').split(/\r?\n/);

    for (const line of lines) {
        const match = line.match(VERSION_HEADER_REGEX);
        if (match) {
            return match[1];
        }
    }

    throw new Error(`Could not find a released version header (e.g. "## [v1.2.3]") in ${filePath}`);
}

function parseSemVer(version) {
    const [core] = version.split('-');
    const parts = core.split('.').map(Number);

    if (parts.length !== 3 || parts.some(Number.isNaN)) {
        throw new Error(`Not a valid MAJOR.MINOR.PATCH version: "${version}"`);
    }

    return parts;
}

function compareSemVer(a, b) {
    const [aMajor, aMinor, aPatch] = parseSemVer(a);
    const [bMajor, bMinor, bPatch] = parseSemVer(b);

    if (aMajor !== bMajor) {
        return aMajor - bMajor;
    }
    if (aMinor !== bMinor) {
        return aMinor - bMinor;
    }
    return aPatch - bPatch;
}

function main() {
    const buildPropsVersion = readVersionFromBuildProps(buildPropsPath);
    const changelogVersion = readNewestVersionFromChangelog(rootChangelogPath);

    if (buildPropsVersion === changelogVersion) {
        log(`Versions match (${buildPropsVersion}). OK.`);
        return;
    }

    const comparison = compareSemVer(changelogVersion, buildPropsVersion);
    const explanation = comparison > 0
        ? `CHANGELOG.md (${changelogVersion}) is newer than Directory.Build.props (${buildPropsVersion}). ` +
        `Did you forget to bump <VersionPrefix> in Directory.Build.props?`
        : `Directory.Build.props (${buildPropsVersion}) is newer than CHANGELOG.md (${changelogVersion}). ` +
        `Did you forget to add a "## [v${buildPropsVersion}]" entry to CHANGELOG.md?`;

    throw new Error(
        `Version mismatch between CHANGELOG.md ("${changelogVersion}") and Directory.Build.props ` +
        `("${buildPropsVersion}"). ${explanation}`
    );
}

main();
