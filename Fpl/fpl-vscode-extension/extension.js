'use strict';

const vscode = require('vscode');
const { LanguageClient } = require('vscode-languageclient');
const utils = require('./utils');
const { createFplTheoriesProvider } = require('./providers');
const { createOrShowWebviewPanel, restoreWebviewPanel } = require('./webviewPanel');

let client;
let outputChannel;
let activationCancelled = false;

async function activate(context) {
    activationCancelled = false;

    if (!outputChannel) {
        outputChannel = vscode.window.createOutputChannel('FPL Log');
    }

    utils.init(outputChannel);

    context.subscriptions.push({
        dispose: () => {
            activationCancelled = true;
        }
    });

    try {
        const platform = process.platform;
        const arch = process.arch;
        const runtimeName = platform + '-' + arch;
        utils.log2Console('running on ' + runtimeName, false);

        const path = require('path');
        const relPathToServerDll = path.join(__dirname, 'dotnet-runtimes', 'FplLsDll', 'FplLS.dll');
        const relPathToDotnetRuntime = path.join(__dirname, 'dotnet-runtimes', runtimeName);
        const relPathToDotnet = path.join(relPathToDotnetRuntime, 'dotnet');

        await utils.acquireDotnetRuntime(runtimeName, relPathToDotnetRuntime);

        if (activationCancelled) {
            return;
        }

        const serverOptions = {
            run: { command: relPathToDotnet, args: [relPathToServerDll] },
            debug: { command: relPathToDotnet, args: [relPathToServerDll] }
        };

        const clientOptions = { documentSelector: [{ scheme: 'file', language: 'fpl' }] };

        client = new LanguageClient('fpl-vscode-extension', 'FPL Language Server', serverOptions, clientOptions);

        const fplTheoriesProvider = createFplTheoriesProvider(client);

        // createTreeView instead of registerTreeDataProvider gives access to
        // onDidExpandElement / onDidCollapseElement for collapse-state memory.
        const treeView = vscode.window.createTreeView('fplTheories', {
            treeDataProvider: fplTheoriesProvider,
            showCollapseAll: true
        });

        // Track expand/collapse so the state survives a manual refresh.
        context.subscriptions.push(
            treeView.onDidExpandElement(event => {
                fplTheoriesProvider.markExpanded(event.element.id);
            })
        );

        context.subscriptions.push(
            treeView.onDidCollapseElement(event => {
                fplTheoriesProvider.markCollapsed(event.element.id);
            })
        );

        context.subscriptions.push(treeView);

        const config = vscode.workspace.getConfiguration('fplExtension');
        const configJson = JSON.stringify(config, null, 2);
        const fs = require('fs');
        const relPathToConfig = path.join(__dirname, 'dotnet-runtimes', 'FplLsDll', 'vsfplconfig.json');
        fs.writeFile(relPathToConfig, configJson, err => {
            if (err) utils.log2Console('Error writing file:' + err.message, true);
        });

        const disposableClient = client.start();

        const disposableCommand = vscode.commands.registerCommand('fpl-vscode-extension.helloWorld', function () {
            vscode.window.showInformationMessage('Hello World from "Formal Proving Language"!');
        });

        // Explicit on-demand refresh command — wired to the ⟳ button in
        // the view title bar via package.json menus/view/title.
        const disposableRefresh = vscode.commands.registerCommand('fpl-vscode-extension.refreshTheories', () => {
            fplTheoriesProvider.refresh();
        });

        const disposableCommand2 = vscode.commands.registerCommand('extension.openFileAtPosition', (filePath, lineNumber, columnNumber) => {
            const openPath = vscode.Uri.file(filePath);
            vscode.workspace.openTextDocument(openPath).then(doc => {
                vscode.window.showTextDocument(doc).then(editor => {
                    const position = new vscode.Position(lineNumber - 1, columnNumber - 1);
                    const range = new vscode.Range(position, position);
                    editor.selection = new vscode.Selection(position, position);
                    editor.revealRange(range);
                });
            });
        });

        const disposableWebview = vscode.commands.registerCommand('fpl-vscode-extension.showWebview', () => {
            createOrShowWebviewPanel(context, client);
        });

        restoreWebviewPanel(context, client);

        // Populate the tree once on activation if an FPL file is already open.
        if (vscode.window.activeTextEditor && vscode.window.activeTextEditor.document.languageId === 'fpl') {
            utils.log2Console('initial treeview refresh', false);
            fplTheoriesProvider.refresh();
        }

        context.subscriptions.push(disposableClient);
        context.subscriptions.push(disposableCommand);
        context.subscriptions.push(disposableCommand2);
        context.subscriptions.push(disposableRefresh);
        context.subscriptions.push(disposableWebview);

        utils.log2Console('Launching "Formal Proving Language", enjoy!', false);
    } catch (error) {
        const errorMsg = 'Installing "Formal Proving Language" failed :-(, report issue on https://github.com/bookofproofs/fpl.net';
        utils.log2Console(errorMsg + ': ' + error, true);
        throw new Error(errorMsg);
    }
}

function deactivate() {
    activationCancelled = true;
    utils.init(null);

    if (outputChannel) {
        outputChannel.dispose();
        outputChannel = undefined;
    }

    if (!client) return undefined;
    return client.stop();
}

module.exports = {
    activate,
    deactivate
};