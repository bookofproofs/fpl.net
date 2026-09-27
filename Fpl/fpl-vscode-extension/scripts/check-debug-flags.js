// Ensures that none of the debug/offline flags in the solution-wide
// Directory.Build.props are set to "true" before publishing the VS Code
// extension. Shipping the extension with any of these flags enabled would
// mean the bundled language server DLLs run in a non-production mode.

'use strict';

const fs = require('fs');
const path = require('path');

const extensionRoot = path.resolve(__dirname, '..');
const solutionDir = path.resolve(extensionRoot, '..');
const propsPath = path.join(solutionDir, 'Directory.Build.props');

const FLAGS_TO_CHECK = [
    'FplIsOffline',
    'FplDebugModeParser',
    'FplDebugModeInterpreter'
];

function log(message) {
    console.log(`[check-debug-flags] ${message}`);
}

function main() {
    if (!fs.existsSync(propsPath)) {
        throw new Error(`Directory.Build.props not found at ${propsPath}`);
    }

    const content = fs.readFileSync(propsPath, 'utf8');
    const offendingFlags = [];

    for (const flag of FLAGS_TO_CHECK) {
        const regex = new RegExp(`<${flag}>\\s*(true|false)\\s*<\\/${flag}>`, 'i');
        const match = content.match(regex);

        if (!match) {
            throw new Error(`Could not find <${flag}> element in ${propsPath}`);
        }

        if (match[1].toLowerCase() === 'true') {
            offendingFlags.push(flag);
        }
    }

    if (offendingFlags.length > 0) {
        throw new Error(
            `Refusing to publish: the following flag(s) in Directory.Build.props are set to "true": ` +
            `${offendingFlags.join(', ')}. Set them all to "false" before publishing.`
        );
    }

    log('All debug/offline flags are set to "false". OK to publish.');
}

main();
