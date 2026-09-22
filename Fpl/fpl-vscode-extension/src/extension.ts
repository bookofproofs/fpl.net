'use strict';

import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import { LanguageClient, LanguageClientOptions, ServerOptions } from 'vscode-languageclient/node';
import * as utils from './utils';
import { createFplTheoriesProvider } from './providers';
import { createOrShowWebviewPanel, restoreWebviewPanel } from './webviewPanel';

let client: LanguageClient | undefined;
let outputChannel: vscode.OutputChannel | undefined;
let activationCancelled = false;

interface IDotnetAcquireResult {
    dotnetPath: string;
}

const DOTNET_ACQUIRE_EXTENSION_ID = 'fpl-vscode-extension';
const DOTNET_VERSION = '8.0';

export async function activate(context: vscode.ExtensionContext): Promise<void> {
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
        utils.log2Console('acquiring shared .NET ' + DOTNET_VERSION + ' runtime via ms-dotnettools.vscode-dotnet-runtime', false);

        const acquireResult = await vscode.commands.executeCommand<IDotnetAcquireResult>(
            'dotnet.acquire',
            { version: DOTNET_VERSION, requestingExtensionId: DOTNET_ACQUIRE_EXTENSION_ID }
        );

        if (!acquireResult?.dotnetPath) {
            throw new Error('Failed to acquire the .NET runtime via ms-dotnettools.vscode-dotnet-runtime.');
        }

        const relPathToDotnet = acquireResult.dotnetPath;
        utils.log2Console('using shared dotnet runtime at ' + relPathToDotnet, false);

        if (activationCancelled) {
            return;
        }

        const relPathToServerDll = path.join(__dirname, 'FplLsDll', 'FplLS.dll');

        const serverOptions: ServerOptions = {
            run: { command: relPathToDotnet, args: [relPathToServerDll] },
            debug: { command: relPathToDotnet, args: [relPathToServerDll] }
        };

        const clientOptions: LanguageClientOptions = { documentSelector: [{ scheme: 'file', language: 'fpl' }] };

        client = new LanguageClient('fpl-vscode-extension', 'FPL Language Server', serverOptions, clientOptions);

        const fplTheoriesProvider = createFplTheoriesProvider(client);

        const treeView = vscode.window.createTreeView('fplTheories', {
            treeDataProvider: fplTheoriesProvider,
            showCollapseAll: true
        });

        context.subscriptions.push(
            treeView.onDidExpandElement(event => {
                if (event.element.id !== undefined) {
                    fplTheoriesProvider.markExpanded(event.element.id);
                }
            })
        );

        context.subscriptions.push(
            treeView.onDidCollapseElement(event => {
                if (event.element.id !== undefined) {
                    fplTheoriesProvider.markCollapsed(event.element.id);
                }
            })
        );

        context.subscriptions.push(treeView);

        const config = vscode.workspace.getConfiguration('fplExtension');
        const configJson = JSON.stringify(config, null, 2);
        const relPathToConfig = path.join(__dirname, 'FplLsDll', 'vsfplconfig.json');
        fs.writeFile(relPathToConfig, configJson, err => {
            if (err) utils.log2Console('Error writing file:' + err.message, true);
        });

        await client.start();

        const disposableCommand = vscode.commands.registerCommand('fpl-vscode-extension.helloWorld', function () {
            vscode.window.showInformationMessage('Hello World from "Formal Proving Language"!');
        });

        const disposableRefresh = vscode.commands.registerCommand('fpl-vscode-extension.refreshTheories', () => {
            fplTheoriesProvider.refresh();
        });

        const disposableCommand2 = vscode.commands.registerCommand('extension.openFileAtPosition', (filePath: string, lineNumber: number, columnNumber: number) => {
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
            if (client) {
                createOrShowWebviewPanel(context, client);
            }
        });

        if (client) {
            restoreWebviewPanel(context, client);
        }

        if (vscode.window.activeTextEditor && vscode.window.activeTextEditor.document.languageId === 'fpl') {
            utils.log2Console('initial treeview refresh', false);
            fplTheoriesProvider.refresh();
        }

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

export function deactivate(): Thenable<void> | undefined {
    activationCancelled = true;
    utils.init(undefined);

    if (outputChannel) {
        outputChannel.dispose();
        outputChannel = undefined;
    }

    if (!client) return undefined;
    return client.stop();
}