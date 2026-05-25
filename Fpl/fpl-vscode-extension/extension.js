// hooks up the functionality of the extension and activates / deactivates it 

'use strict';

const vscode = require('vscode');
const { LanguageClient } = require('vscode-languageclient');
const utils = require('./utils');
const { createFplTheoriesProvider, createValidStmtsProvider } = require('./providers');

let client;

function activate(context) {
    const outputChannel = vscode.window.createOutputChannel('FPL Log');
    utils.init(outputChannel);
    try {
        const platform = process.platform;
        const arch = process.arch;
        const runtimeName = platform + '-' + arch;
        utils.log2Console('running on ' + runtimeName, false);

        const path = require('path');
        const relPathToServerDll = path.join(__dirname, 'dotnet-runtimes', 'FplLsDll', 'FplLS.dll');
        const relPathToDotnetRuntime = path.join(__dirname, 'dotnet-runtimes', runtimeName);
        const relPathToDotnet = path.join(relPathToDotnetRuntime, 'dotnet');

        const acquirePromise = utils.acquireDotnetRuntime(runtimeName, relPathToDotnetRuntime);

        acquirePromise.then(() => {
            const serverOptions = {
                run: { command: relPathToDotnet, args: [relPathToServerDll] },
                debug: { command: relPathToDotnet, args: [relPathToServerDll] }
            };

            const clientOptions = { documentSelector: [{ scheme: 'file', language: 'fpl' }] };

            client = new LanguageClient('fpl-vscode-extension', 'FPL Language Server', serverOptions, clientOptions);

            const fplTheoriesProvider = createFplTheoriesProvider(client);
            const fplValidStmtsProvider = createValidStmtsProvider(client);

            vscode.window.registerTreeDataProvider('fplTheories', fplTheoriesProvider);
            vscode.window.registerTreeDataProvider('fplValidStmts', fplValidStmtsProvider);

            vscode.window.onDidChangeActiveTextEditor((editor) => {
                utils.log2Console('onDidChangeActiveTextEditor', false);
                if (editor && editor.document.languageId === 'fpl') {
                    fplTheoriesProvider.refresh();
                    fplValidStmtsProvider.refresh();
                }
            });

            vscode.workspace.onDidChangeTextDocument((event) => {
                if (event.document.languageId === 'fpl') {
                    fplTheoriesProvider.refresh();
                    fplValidStmtsProvider.refresh();
                }
            });

            const config = vscode.workspace.getConfiguration('fplExtension');
            const configJson = JSON.stringify(config, null, 2);
            const fs = require('fs');
            const relPathToConfig = path.join(__dirname, 'dotnet-runtimes', 'FplLsDll', 'vsfplconfig.json');
            fs.writeFile(relPathToConfig, configJson, (err) => {
                if (err) utils.log2Console('Error writing file:' + err.message, true);
            });

            const disposableClient = client.start();

            const disposableCommand = vscode.commands.registerCommand('fpl-vscode-extension.helloWorld', function () {
                vscode.window.showInformationMessage('Hello World from "Formal Proving Language"!');
            });

            if (vscode.window.activeTextEditor && vscode.window.activeTextEditor.document.languageId === 'fpl') {
                utils.log2Console('initial treeview refresh', false);
                fplTheoriesProvider.refresh();
                fplValidStmtsProvider.refresh();
            }

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

            context.subscriptions.push(disposableClient);
            context.subscriptions.push(disposableCommand);
            context.subscriptions.push(disposableCommand2);

            utils.log2Console('Launching "Formal Proving Language", enjoy!', false);
        });

    } catch (error) {
        const errorMsg = 'Installing "Formal Proving Language" failed :-(, report issue on https://github.com/bookofproofs/fpl.net';
        utils.log2Console(errorMsg, true);
        throw new Error(errorMsg);
    }
}

function deactivate() {
    if (!client) return undefined;
    return client.stop();
}

module.exports = {
    activate,
    deactivate
};