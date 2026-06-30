// Manages the custom FPL Webview panel lifecycle and data binding

'use strict';

const vscode = require('vscode');
const utils = require('./utils');

let currentPanel = undefined;

function createOrShowWebviewPanel(context, client) {
    const column = vscode.window.activeTextEditor
        ? vscode.window.activeTextEditor.viewColumn
        : vscode.ViewColumn.One;

    if (currentPanel) {
        currentPanel.reveal(column);
        refreshWebviewData(client);
        return;
    }

    currentPanel = vscode.window.createWebviewPanel(
        'fplDataView',
        'FPL Data View',
        column,
        {
            enableScripts: true,
            retainContextWhenHidden: true
        }
    );

    currentPanel.webview.html = getWebviewContent();

    refreshWebviewData(client);

    currentPanel.webview.onDidReceiveMessage(
        message => {
            if (message.command === 'refresh') {
                refreshWebviewData(client);
            }
        },
        undefined,
        context.subscriptions
    );

    currentPanel.onDidDispose(
        () => { currentPanel = undefined; },
        null,
        context.subscriptions
    );
}

function refreshWebviewData(client) {
    if (!currentPanel) {
        return;
    }
    client.sendRequest('getWebviewData', {}).then(json => {
        currentPanel.webview.postMessage({ command: 'update', data: json });
    }).catch(err => {
        utils.log2Console('Webview data fetch failed: ' + err, true);
        currentPanel.webview.postMessage({ command: 'error', message: String(err) });
    });
}

function getWebviewContent() {
    return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>FPL Valid Statements Overview</title>
    <style>
        body {
            font-family: var(--vscode-font-family);
            color: var(--vscode-foreground);
            background-color: var(--vscode-editor-background);
            padding: 12px;
        }
        h2 { margin-top: 0; }
        button {
            background: var(--vscode-button-background);
            color: var(--vscode-button-foreground);
            border: none;
            padding: 6px 14px;
            cursor: pointer;
            border-radius: 2px;
        }
        button:hover { background: var(--vscode-button-hoverBackground); }
        #status {
            color: var(--vscode-descriptionForeground);
            font-style: italic;
            margin: 8px 0;
        }
        pre {
            background: var(--vscode-textBlockQuote-background);
            padding: 10px;
            overflow: auto;
            white-space: pre-wrap;
            word-break: break-all;
        }
    </style>
</head>
<body>
    <h2>Valid Statements Overview</h2>
    <button onclick="refresh()">&#x27F3; Refresh</button>
    <p id="status">Loading&hellip;</p>
    <pre id="content"></pre>
    <script>
        const vscode = acquireVsCodeApi();

        function refresh() {
            document.getElementById('status').textContent = 'Loading\u2026';
            vscode.postMessage({ command: 'refresh' });
        }

        window.addEventListener('message', event => {
            const message = event.data;
            if (message.command === 'update') {
                document.getElementById('status').textContent =
                    'Last updated: ' + new Date().toLocaleTimeString();
                try {
                    const parsed = JSON.parse(message.data);
                    document.getElementById('content').textContent =
                        JSON.stringify(parsed, null, 2);
                } catch (_) {
                    document.getElementById('content').textContent = message.data;
                }
            } else if (message.command === 'error') {
                document.getElementById('status').textContent = 'Error: ' + message.message;
            }
        });
    </script>
</body>
</html>`;
}

module.exports = { createOrShowWebviewPanel };
