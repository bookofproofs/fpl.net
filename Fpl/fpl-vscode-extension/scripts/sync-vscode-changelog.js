// Regenerates the VS Code extension's CHANGELOG.md from the solution-wide root CHANGELOG.md,
// keeping only entries tagged **[vscode]**, so that `vsce package` / `vsce publish` always ships
// an up-to-date, extension-specific change log.

'use strict';

const fs = require('fs');
const path = require('path');

const extensionRoot = path.resolve(__dirname, '..');
const solutionDir = path.resolve(extensionRoot, '..');
const rootChangelogPath = path.join(solutionDir, '..', 'CHANGELOG.md');
const extensionChangelogPath = path.join(extensionRoot, 'CHANGELOG.md');

const VSCODE_TAG = '[vscode]';

const PREAMBLE = `# Change Log

All notable changes to the VS Code extension **FPL (Formal Proving Language)**
are documented in this file. For a full list of changes of the **FPL solution**
(including parser, interpreter, language server, ...),
see [CHANGELOG.md](https://github.com/bookofproofs/fpl.net/blob/main/CHANGELOG.md).

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to Semantic Versioning (MAJOR.MINOR.PATCH).

`;

const VERSION_HEADER_REGEX = /^## \[.+\]/;
const SECTION_HEADER_REGEX = /^### (.+)/;
const BULLET_TAG_REGEX = /^- \*\*\[(\w[\w-]*)\]\*\*/;

function log(message) {
    console.log(`[sync-vscode-changelog] ${message}`);
}

function parseVersions(lines) {
    const versions = [];
    let currentVersion = null;
    let currentSection = null;

    for (const line of lines) {
        const versionMatch = line.match(VERSION_HEADER_REGEX);
        if (versionMatch) {
            currentVersion = { header: line, sections: [] };
            versions.push(currentVersion);
            currentSection = null;
            continue;
        }

        if (!currentVersion) {
            continue;
        }

        const sectionMatch = line.match(SECTION_HEADER_REGEX);
        if (sectionMatch) {
            currentSection = { header: line, bullets: [] };
            currentVersion.sections.push(currentSection);
            continue;
        }

        const bulletMatch = line.match(BULLET_TAG_REGEX);
        if (bulletMatch && currentSection) {
            currentSection.bullets.push({ tag: bulletMatch[1], line });
        }
    }

    return versions;
}

function filterVscodeVersions(versions) {
    const filtered = [];

    for (const version of versions) {
        const sections = version.sections
            .map((section) => ({
                header: section.header,
                bullets: section.bullets.filter((b) => `[${b.tag}]` === VSCODE_TAG)
            }))
            .filter((section) => section.bullets.length > 0);

        if (sections.length > 0) {
            filtered.push({ header: version.header, sections });
        }
    }

    return filtered;
}

function renderChangelog(versions) {
    const parts = [PREAMBLE.trimEnd()];

    for (const version of versions) {
        parts.push('');
        parts.push(version.header);
        for (const section of version.sections) {
            parts.push(section.header);
            for (const bullet of section.bullets) {
                parts.push(bullet.line);
            }
        }
    }

    return parts.join('\n') + '\n';
}

function main() {
    if (!fs.existsSync(rootChangelogPath)) {
        throw new Error(`Root CHANGELOG.md not found at ${rootChangelogPath}`);
    }

    const rootContent = fs.readFileSync(rootChangelogPath, 'utf8');
    const lines = rootContent.split(/\r?\n/);

    const versions = parseVersions(lines);
    const vscodeVersions = filterVscodeVersions(versions);

    if (vscodeVersions.length === 0) {
        log('No [vscode]-tagged entries found in root CHANGELOG.md; nothing to write.');
        return;
    }

    const newContent = renderChangelog(vscodeVersions);
    fs.writeFileSync(extensionChangelogPath, newContent, 'utf8');
    log(`Wrote ${vscodeVersions.length} version(s) to ${extensionChangelogPath}`);
}

main();
