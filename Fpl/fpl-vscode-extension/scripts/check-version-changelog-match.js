// Verifies that the newest released version in the root CHANGELOG.md matches the version
// defined in Directory.Build.props (VersionPrefix), that the newest version in RELEASE_NOTES.md
// matches it too, and that the release dates of both the newest CHANGELOG.md and RELEASE_NOTES.md
// entries correspond to the current date. Throws and aborts the publish process if any of these
// checks fail, since that indicates the changelog, release notes, or version was forgotten to be
// updated/bumped.

'use strict';

const fs = require('fs');
const path = require('path');

const extensionRoot = path.resolve(__dirname, '..');
const solutionDir = path.resolve(extensionRoot, '..');
const repoRoot = path.resolve(solutionDir, '..');

const buildPropsPath = path.join(solutionDir, 'Directory.Build.props');
const rootChangelogPath = path.join(repoRoot, 'CHANGELOG.md');
const releaseNotesPath = path.join(solutionDir, 'RELEASE_NOTES.md');

const VERSION_HEADER_REGEX = /^## \[v?([0-9]+\.[0-9]+\.[0-9]+(?:-[0-9A-Za-z.-]+)?)\]\s*-\s*(\d{4}-\d{2}-\d{2})/;

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

function readNewestVersionAndDate(filePath) {
    if (!fs.existsSync(filePath)) {
        throw new Error(`File not found at ${filePath}`);
    }

    const lines = fs.readFileSync(filePath, 'utf8').split(/\r?\n/);

    for (const line of lines) {
        const match = line.match(VERSION_HEADER_REGEX);
        if (match) {
            return { version: match[1], date: match[2] };
        }
    }

    throw new Error(`Could not find a released version header (e.g. "## [v1.2.3] - 2024-01-01") in ${filePath}`);
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

function todayIsoDate() {
    const now = new Date();
    const year = now.getFullYear();
    const month = String(now.getMonth() + 1).padStart(2, '0');
    const day = String(now.getDate()).padStart(2, '0');
    return `${year}-${month}-${day}`;
}

function main() {
    const buildPropsVersion = readVersionFromBuildProps(buildPropsPath);
    const changelogEntry = readNewestVersionAndDate(rootChangelogPath);
    const releaseNotesEntry = readNewestVersionAndDate(releaseNotesPath);

    if (buildPropsVersion !== changelogEntry.version) {
        const comparison = compareSemVer(changelogEntry.version, buildPropsVersion);
        const explanation = comparison > 0
            ? `CHANGELOG.md (${changelogEntry.version}) is newer than Directory.Build.props (${buildPropsVersion}). ` +
            `Did you forget to bump <VersionPrefix> in Directory.Build.props?`
            : `Directory.Build.props (${buildPropsVersion}) is newer than CHANGELOG.md (${changelogEntry.version}). ` +
            `Did you forget to add a "## [v${buildPropsVersion}]" entry to CHANGELOG.md?`;

        throw new Error(
            `Version mismatch between CHANGELOG.md ("${changelogEntry.version}") and Directory.Build.props ` +
            `("${buildPropsVersion}"). ${explanation}`
        );
    }

    if (buildPropsVersion !== releaseNotesEntry.version) {
        const comparison = compareSemVer(releaseNotesEntry.version, buildPropsVersion);
        const explanation = comparison > 0
            ? `RELEASE_NOTES.md (${releaseNotesEntry.version}) is newer than Directory.Build.props (${buildPropsVersion}). ` +
            `Did you forget to bump <VersionPrefix> in Directory.Build.props?`
            : `Directory.Build.props (${buildPropsVersion}) is newer than RELEASE_NOTES.md (${releaseNotesEntry.version}). ` +
            `Did you forget to add a "## [${buildPropsVersion}]" entry to RELEASE_NOTES.md?`;

        throw new Error(
            `Version mismatch between RELEASE_NOTES.md ("${releaseNotesEntry.version}") and Directory.Build.props ` +
            `("${buildPropsVersion}"). ${explanation}`
        );
    }

    const today = todayIsoDate();

    if (changelogEntry.date !== today) {
        throw new Error(
            `The newest CHANGELOG.md entry ("## [${changelogEntry.version}] - ${changelogEntry.date}") is not ` +
            `dated today (${today}). Did you forget to update the change date?`
        );
    }

    if (releaseNotesEntry.date !== today) {
        throw new Error(
            `The newest RELEASE_NOTES.md entry ("## [${releaseNotesEntry.version}] - ${releaseNotesEntry.date}") is ` +
            `not dated today (${today}). Did you forget to update the release date?`
        );
    }

    log(`Versions match (${buildPropsVersion}) and release dates correspond to today (${today}). OK.`);
}

main();
