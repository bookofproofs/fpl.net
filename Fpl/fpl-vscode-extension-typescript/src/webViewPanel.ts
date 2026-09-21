// Manages the custom FPL Webview panel lifecycle, data binding and layout persistence

'use strict';

import * as vscode from 'vscode';
import { LanguageClient } from 'vscode-languageclient/node';
import * as utils from './utils';

const STATE_KEY = 'fplWebviewLayout';
let currentPanel: vscode.WebviewPanel | undefined;

interface WebviewLayoutState {
    column: vscode.ViewColumn;
    isOpen: boolean;
}

interface WebviewMessage {
    command: 'refresh' | 'navigate';
    filePath?: string;
    line?: number;
    column?: number;
}

function loadLayout(context: vscode.ExtensionContext): WebviewLayoutState {
    return context.workspaceState.get<WebviewLayoutState>(STATE_KEY, { column: vscode.ViewColumn.Two, isOpen: false });
}

function saveLayout(context: vscode.ExtensionContext, column: vscode.ViewColumn | null, isOpen: boolean): void {
    const current = loadLayout(context);
    context.workspaceState.update(STATE_KEY, {
        column: column != null ? column : current.column,
        isOpen
    });
}

export function createOrShowWebviewPanel(context: vscode.ExtensionContext, client: LanguageClient): void {
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
        column as vscode.ViewColumn,
        {
            enableScripts: true,
            retainContextWhenHidden: true,
            localResourceRoots: [
                vscode.Uri.joinPath(context.extensionUri, 'node_modules', 'katex', 'dist')
            ]
        }
    );

    const katexBase = vscode.Uri.joinPath(context.extensionUri, 'node_modules', 'katex', 'dist');
    const katexJs = currentPanel.webview.asWebviewUri(vscode.Uri.joinPath(katexBase, 'katex.min.js'));
    const katexCss = currentPanel.webview.asWebviewUri(vscode.Uri.joinPath(katexBase, 'katex.min.css'));

    currentPanel.webview.html = getWebviewContent(katexJs, katexCss);

    saveLayout(context, column as vscode.ViewColumn, true);

    refreshWebviewData(client);

    currentPanel.webview.onDidReceiveMessage(
        (message: WebviewMessage) => {
            if (message.command === 'refresh') {
                refreshWebviewData(client);
            } else if (message.command === 'navigate' && message.filePath !== undefined && message.line !== undefined && message.column !== undefined) {
                const uri = vscode.Uri.file(message.filePath);
                vscode.workspace.openTextDocument(uri).then(doc => {
                    vscode.window.showTextDocument(doc, vscode.ViewColumn.One).then(editor => {
                        // FParsec positions are 1-based; VSCode Position is 0-based
                        const pos = new vscode.Position(
                            Math.max(0, (message.line as number) - 1),
                            Math.max(0, (message.column as number) - 1)
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

export function restoreWebviewPanel(context: vscode.ExtensionContext, client: LanguageClient): void {
    const layout = loadLayout(context);
    utils.log2Console('Restoring webview layout: ' + JSON.stringify(layout), false);
    if (layout.isOpen) {
        createOrShowWebviewPanel(context, client);
    }
}

function refreshWebviewData(client: LanguageClient): void {
    if (!currentPanel) {
        return;
    }
    client.sendRequest<string>('getWebviewData', {}).then(json => {
        currentPanel?.webview.postMessage({ command: 'update', data: json });
    }).catch(err => {
        utils.log2Console('Webview data fetch failed: ' + err, true);
        currentPanel?.webview.postMessage({ command: 'error', message: String(err) });
    });
}

function getWebviewContent(katexJs: vscode.Uri, katexCss: vscode.Uri): string {
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
            { key: 'FilePath',            label: 'Path',   hidden: true },
            { key: 'Line',                label: 'Line',   hidden: true },
            { key: 'Column',              label: 'Column', hidden: true },
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