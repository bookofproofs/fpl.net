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
            retainContextWhenHidden: true,
            localResourceRoots: [
                vscode.Uri.joinPath(context.extensionUri, 'node_modules', 'katex', 'dist')
            ]
        }
    );

    const katexBase = vscode.Uri.joinPath(context.extensionUri, 'node_modules', 'katex', 'dist');
    const katexJs  = currentPanel.webview.asWebviewUri(vscode.Uri.joinPath(katexBase, 'katex.min.js'));
    const katexCss = currentPanel.webview.asWebviewUri(vscode.Uri.joinPath(katexBase, 'katex.min.css'));

    currentPanel.webview.html = getWebviewContent(katexJs, katexCss);

    saveLayout(context, column, true);

    refreshWebviewData(client);

    currentPanel.webview.onDidReceiveMessage(
        message => {
            if (message.command === 'refresh') {
                refreshWebviewData(client);
            } else if (message.command === 'navigate') {
                const uri = vscode.Uri.file(message.filePath);
                vscode.workspace.openTextDocument(uri).then(doc => {
                    vscode.window.showTextDocument(doc, vscode.ViewColumn.One).then(editor => {
                        // FParsec positions are 1-based; VSCode Position is 0-based
                        const pos = new vscode.Position(
                            Math.max(0, message.line - 1),
                            Math.max(0, message.column - 1)
                        );
                        editor.selection = new vscode.Selection(pos, pos);
                        editor.revealRange(
                            new vscode.Range(pos, pos),
                            vscode.TextEditorRevealType.InCenterIfOutsideViewport
                        );
                    });
                });
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

function getWebviewContent(katexJs, katexCss) {
    return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>FPL Valid Statements Overview</title>
    <link rel="stylesheet" href="${katexCss}">
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
        #content {
            overflow-x: auto;
        }
        table {
            border-collapse: collapse;
            width: 100%;
            min-width: 600px;
        }
        thead tr {
            background-color: var(--vscode-editor-lineHighlightBackground);
        }
        th {
            padding: 6px 10px;
            text-align: left;
            cursor: pointer;
            user-select: none;
            white-space: nowrap;
            border-bottom: 2px solid var(--vscode-panel-border);
            position: relative;
        }
        th:hover {
            background-color: var(--vscode-list-hoverBackground);
        }   
        thead th:first-child {
            text-align: center;
        }
        th.sort-asc::after  { content: ' \\25B2'; font-size: 0.75em; }
        th.sort-desc::after { content: ' \\25BC'; font-size: 0.75em; }
        td {
            padding: 5px 10px;
            border-bottom: 1px solid var(--vscode-panel-border);
            vertical-align: middle;
            word-break: break-word;
            max-width: 320px;
        }
        td.expr-cell {
            white-space: nowrap;
            text-align: center;
        }
        tbody tr {
            cursor: pointer;
        }
        tbody tr:hover {
            background-color: var(--vscode-list-hoverBackground);
        }
        .empty {
            color: var(--vscode-descriptionForeground);
            font-style: italic;
            padding: 8px 0;
        }
        /* KaTeX colour inherits from the VSCode theme foreground */
        .katex { color: var(--vscode-foreground); }
    </style>
</head>
<body>
    <h2>Valid Statements Overview</h2>
    <button onclick="refresh()">&#x27F3; Refresh</button>
    <p id="status">Loading&hellip;</p>
    <div id="content"></div>
    <script src="${katexJs}"></script>
    <script>
        const vscode = acquireVsCodeApi();
        const COLUMNS = [
            { key: 'statementExpression', label: 'Logical Expression' },
            { key: 'reason',              label: 'Source'             },
            { key: 'blockName',           label: 'Block'              },
            { key: 'theoryName',          label: 'Theory'             },
            { key: 'FilePath',            label: 'Path'               },
            { key: 'Line',                label: 'Line'               },
            { key: 'Column',              label: 'Column'             },
        ];
        
        let _rows = [];
        let _sortCol = null;
        let _sortAsc = true;

        // ── Unicode → LaTeX conversion ────────────────────────────────────────
        // Maps every FPL Unicode symbol to its KaTeX equivalent.
        const UNICODE_TO_LATEX = [
            // logical connectives
            ['⇒',  '\\\\Rightarrow'],
            ['⇔',  '\\\\Leftrightarrow'],
            ['¬',  '\\\\neg '],
            ['∧',  '\\\\land'],
            ['∨',  '\\\\lor'],
            // quantifiers  (order matters: ∃! before ∃)
            ['∃!', '\\\\exists!'],
            ['∃',  '\\\\exists'],
            ['∀',  '\\\\forall'],
            // equality / membership
            ['≠',  '\\\\neq'],
            ['∈',  '\\\\in'],
            ['∉',  '\\\\notin'],
            ['⊆',  '\\\\subseteq'],
            ['⊂',  '\\\\subset'],
        ];

        /**
         * Converts a FPL Unicode expression string to a KaTeX-renderable LaTeX string.
         * Inference rules use "/" as numerator/denominator separator and are rendered
         * as a fraction: \dfrac{premises}{conclusion}.
         *
         * @param {string} expr - the raw statementExpression value from ToJson2()
         * @returns {string}    - a LaTeX string suitable for katex.renderToString()
         */
        function fplToLatex(expr) {
            const slashIdx = expr.indexOf('/');
            if (slashIdx !== -1) {
                // Inference rule: "premise1, premise2 / conclusion"
                const num = expr.slice(0, slashIdx).trim();
                const den = expr.slice(slashIdx + 1).trim();
                return \`\\\\dfrac{\${applySymbols(num)}}{\${applySymbols(den)}}\`;
            }
            return applySymbols(expr);
        }

        function applySymbols(str) {
            let result = str;
            for (const [unicode, latex] of UNICODE_TO_LATEX) {
                result = result.split(unicode).join(latex);
            }
            return result;
        }

        /**
         * Renders a FPL expression as HTML using KaTeX.
         * Falls back to the raw escaped string if KaTeX throws.
         *
         * @param {string} expr
         * @returns {string} - HTML string
         */
        function renderExpr(expr) {
            if (!expr) { return ''; }
            try {
                return katex.renderToString(fplToLatex(expr), {
                    throwOnError: false,
                    displayMode: false,
                    output: 'html'
                });
            } catch (_) {
                return esc(expr);
            }
        }

        // ── General helpers ───────────────────────────────────────────────────
        function esc(s) {
            return String(s ?? '')
                .replace(/&/g, '&amp;')
                .replace(/</g, '&lt;')
                .replace(/>/g, '&gt;');
        }

        function buildTable(rows) {
            if (!rows || rows.length === 0) {
                return '<p class="empty">No valid statements found.</p>';
            }

            const headers = COLUMNS.map(col => {
                let cls = '';
                if (_sortCol === col.key) { cls = _sortAsc ? ' class="sort-asc"' : ' class="sort-desc"'; }
                return \`<th\${cls} onclick="sortBy('\${col.key}')">\${esc(col.label)}</th>\`;
            }).join('');

            const bodyRows = rows.map(row => {
                const cells = COLUMNS.map(col => {
                    if (col.key === 'statementExpression') {
                        return \`<td class="expr-cell">\${renderExpr(row[col.key])}</td>\`;
                    }
                    return \`<td>\${esc(row[col.key])}</td>\`;
                }).join('');
                return \`<tr data-filepath="\${esc(row['FilePath'])}" data-line="\${row['Line']}" data-column="\${row['Column']}">\${cells}</tr>\`;
            }).join('');

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

        // ── Row double-click → navigate in editor ─────────────────────────────
        document.getElementById('content').addEventListener('dblclick', e => {
            const tr = e.target.closest('tr[data-filepath]');
            if (!tr) { return; }
            vscode.postMessage({
                command: 'navigate',
                filePath: tr.dataset.filepath,
                line: parseInt(tr.dataset.line, 10),
                column: parseInt(tr.dataset.column, 10)
            });
        });
    </script>
</body>
</html>`;
}

module.exports = { createOrShowWebviewPanel, restoreWebviewPanel };
