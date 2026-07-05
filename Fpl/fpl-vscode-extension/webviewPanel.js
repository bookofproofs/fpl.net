// Manages the custom FPL Webview panel lifecycle, data binding and layout persistence

'use strict';

const vscode = require('vscode');
const utils = require('./utils');

const STATE_KEY = 'fplWebviewLayout';
let currentPanel = undefined;

function loadLayout(context) {
    return context.workspaceState.get(STATE_KEY, { column: vscode.ViewColumn.Two, isOpen: false });
}

function saveLayout(context, column, isOpen) {
    const current = loadLayout(context);
    context.workspaceState.update(STATE_KEY, {
        column: column != null ? column : current.column,
        isOpen
    });
}

function createOrShowWebviewPanel(context, client) {
    const layout = loadLayout(context);
    const column = layout.column
        || (vscode.window.activeTextEditor
            ? vscode.window.activeTextEditor.viewColumn
            : vscode.ViewColumn.Two);

    if (currentPanel) {
        currentPanel.reveal(column);
        refreshWebviewData(client);
        return;
    }

    currentPanel = vscode.window.createWebviewPanel(
        'fplDataView',
        'Valid Statements Overview',
        column,
        {
            enableScripts: true,
            retainContextWhenHidden: true
        }
    );

    currentPanel.webview.html = getWebviewContent();

    saveLayout(context, column, true);

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

    // Track column changes when the user drags the panel to a different position
    currentPanel.onDidChangeViewState(
        e => {
            if (e.webviewPanel.viewColumn != null) {
                saveLayout(context, e.webviewPanel.viewColumn, true);
            }
        },
        null,
        context.subscriptions
    );

    // Only fires when the user explicitly closes the panel via the X button
    currentPanel.onDidDispose(
        () => {
            saveLayout(context, null, false);
            currentPanel = undefined;
        },
        null,
        context.subscriptions
    );
}

function restoreWebviewPanel(context, client) {
    const layout = loadLayout(context);
    utils.log2Console('Restoring webview layout: ' + JSON.stringify(layout), false);
    if (layout.isOpen) {
        createOrShowWebviewPanel(context, client);
    }
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
            font-size: var(--vscode-font-size);
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
    <div id="content"></div>
    <script>
        const vscode = acquireVsCodeApi();
        const COLUMNS = ['statementExpression', 'reason', 'nodeName', 'FilePath', 'Line', 'Column'];

        let _rows = [];
        let _sortCol = null;
        let _sortAsc = true;

        function refresh() {
            document.getElementById('status').textContent = 'Loading\u2026';
            vscode.postMessage({ command: 'refresh' });
        }

        function buildTable(rows) {
            if (!rows || rows.length === 0) {
                return '<p class="empty">No valid statements found.</p>';
            }

            const esc = s => String(s ?? '')
                .replace(/&/g, '&amp;')
                .replace(/</g, '&lt;')
                .replace(/>/g, '&gt;');

            const headers = COLUMNS.map(col => {
                let cls = '';
                if (_sortCol === col) cls = _sortAsc ? ' class="sort-asc"' : ' class="sort-desc"';
                return \`<th\${cls} onclick="sortBy('\${col}')">\${esc(col)}</th>\`;
            }).join('');

            const bodyRows = rows.map(row =>
                '<tr>' + COLUMNS.map(col => \`<td>\${esc(row[col])}</td>\`).join('') + '</tr>'
            ).join('');

            return \`<table><thead><tr>\${headers}</tr></thead><tbody>\${bodyRows}</tbody></table>\`;
        }

        function sortBy(col) {
            if (_sortCol === col) {
                _sortAsc = !_sortAsc;
            } else {
                _sortCol = col;
                _sortAsc = true;
            }

            const sorted = [..._rows].sort((a, b) => {
                const av = String(a[col] ?? '').toLowerCase();
                const bv = String(b[col] ?? '').toLowerCase();
                if (av < bv) return _sortAsc ? -1 : 1;
                if (av > bv) return _sortAsc ? 1 : -1;
                return 0;
            });

            document.getElementById('content').innerHTML = buildTable(sorted);
        }

        window.addEventListener('message', event => {
            const message = event.data;
            if (message.command === 'update') {
                document.getElementById('status').textContent =
                    'Last updated: ' + new Date().toLocaleTimeString();
                try {
                    _rows = JSON.parse(message.data);
                    _sortCol = null;
                    _sortAsc = true;
                    document.getElementById('content').innerHTML = buildTable(_rows);
                } catch (_) {
                    document.getElementById('content').innerHTML =
                        '<p class="empty">Failed to parse data.</p>';
                }
            } else if (message.command === 'error') {
                document.getElementById('status').textContent = 'Error: ' + message.message;
            }
        });
    </script>
</body>
</html>`;
}

module.exports = { createOrShowWebviewPanel, restoreWebviewPanel };
