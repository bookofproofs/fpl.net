// Defines tree view items for the extension

'use strict';

import * as vscode from 'vscode';

// Raw JSON shape of a scope/arglist entry as returned by the 'getTreeData'
// language server request (before being wrapped into a MyTreeItem).
export interface FplScopeItemJson {
    Type: string;
    Name: string;
    Line: number;
    Column: number;
    FilePath: string;
    FplValueType: string;
    FplValueRepr: string;
    FplRefersTo: string;
    Scope: FplScopeItemJson[];
    ArgList: FplScopeItemJson[];
}

export const typeToIconMap: Map<string, string> = new Map();
typeToIconMap.set('th', 'library');
typeToIconMap.set('var', 'variable');
typeToIconMap.set('*var', 'bracket-error');
typeToIconMap.set('+var', 'bracket-dot');
typeToIconMap.set('mpred', 'symbol-boolean');
typeToIconMap.set('opred', 'symbol-boolean');
typeToIconMap.set('and', 'symbol-boolean');
typeToIconMap.set('or', 'symbol-boolean');
typeToIconMap.set('xor', 'symbol-boolean');
typeToIconMap.set('impl', 'symbol-boolean');
typeToIconMap.set('iif', 'symbol-boolean');
typeToIconMap.set('not', 'symbol-boolean');
typeToIconMap.set('=', 'symbol-boolean');
typeToIconMap.set('is', 'symbol-boolean');
typeToIconMap.set('mfunc', 'symbol-interface');
typeToIconMap.set('ofunc', 'symbol-interface');
typeToIconMap.set('ctor', 'symbol-constructor');
typeToIconMap.set('def cl', 'symbol-class');
typeToIconMap.set('obj', 'primitive-square');
typeToIconMap.set('loc', 'location');
typeToIconMap.set('thm', 'layout-panel-justify');
typeToIconMap.set('lem', 'layout-panel-center');
typeToIconMap.set('prop', 'layout-panel-right');
typeToIconMap.set('cor', 'layout-sidebar-right');
typeToIconMap.set('prf', 'testing-passed-icon');
typeToIconMap.set('conj', 'workspace-unknown');
typeToIconMap.set('ax', 'layout');
typeToIconMap.set('inf', 'symbol-structure');
typeToIconMap.set('qtr', 'symbol-boolean');
typeToIconMap.set('def pred', 'symbol-boolean');
typeToIconMap.set('pred', 'symbol-boolean');
typeToIconMap.set('def func', 'symbol-interface');
typeToIconMap.set('func', 'symbol-interface');
typeToIconMap.set('ref', 'link');
typeToIconMap.set('arg', 'indent');
typeToIconMap.set('just', 'kebab-horizontal');
typeToIconMap.set('ainf', 'kebab-vertical');
typeToIconMap.set('lang', 'globe');
typeToIconMap.set('trsl', 'symbol-text');
typeToIconMap.set('map', 'preview');
typeToIconMap.set('stmt', 'symbol-event');
typeToIconMap.set('decr', 'symbol-event');
typeToIconMap.set('ass', 'target');
typeToIconMap.set('def ext', 'extensions');
typeToIconMap.set('inst', 'output');
typeToIconMap.set('ind', 'symbol-numeric');
typeToIconMap.set('tpl', 'gear');
typeToIconMap.set('undef', 'question');
typeToIconMap.set('undet', 'question');

export class MyTreeItem extends vscode.TreeItem {
    public typ: string;
    public lineNumber: number;
    public columnNumber: number;
    public filePath: string;
    public isVirtual: boolean;
    public scope: FplScopeItemJson[];
    public arglist: FplScopeItemJson[];

    constructor(
        typ: string,
        inScope: number,
        label: string,
        lineNumber: number,
        columnNumber: number,
        filePath: string,
        fplValueType: string,
        fplValueRepr: string,
        fplRefersTo: string,
        scope: FplScopeItemJson[] = [],
        arglist: FplScopeItemJson[] = []
    ) {
        super(label, scope.length > 0 || arglist.length > 0 ? vscode.TreeItemCollapsibleState.Collapsed : vscode.TreeItemCollapsibleState.None);
        this.typ = typ;
        this.lineNumber = lineNumber;
        this.columnNumber = columnNumber;
        this.filePath = filePath;
        if (typ === 'mpred') {
            this.label = 'pred prop ' + label;
        } else if (typ === 'mfunc') {
            this.label = 'func prop ' + label;
        } else {
            this.label = typ + ' ' + label;
        }

        // Stable id used by the collapse-state manager; composed from
        // fields that identify the same logical node across reloads.
        this.id = `${filePath}|${typ}|${label}`;

        this.isVirtual = false;
        const markdownTooltip = new vscode.MarkdownString();
        markdownTooltip.isTrusted = true;
        if (label !== '') markdownTooltip.appendMarkdown(`📌 **Name:** ${label}\n\n`);
        if (fplValueType !== '') markdownTooltip.appendMarkdown(`🧩 **Type:** ${fplValueType}\n\n`);
        if (fplRefersTo !== '') markdownTooltip.appendMarkdown(`👉 **Refers to:** ${fplRefersTo}\n\n`);
        if (fplValueRepr !== '' && fplValueRepr !== 'None') markdownTooltip.appendMarkdown(`🧩 **Value:** ${fplValueRepr}\n\n`);
        this.tooltip = markdownTooltip;
        this.scope = scope;
        this.arglist = arglist;

        if (inScope === 1) {
            this.iconPath = this.getIconPathWithColor(typeToIconMap.get(typ) || 'default-view-icon', 'textPreformat.foreground');
        } else if (inScope === 2) {
            this.iconPath = this.getIconPathWithColor(typeToIconMap.get(typ) || 'default-view-icon', 'focusBorder');
        } else {
            this.iconPath = this.getIconPathWithColor(typeToIconMap.get(typ) || 'default-view-icon', 'textSeparator.foreground');
        }

        this.command = {
            command: 'extension.openFileAtPosition',
            title: 'Open File',
            arguments: [this.filePath, this.lineNumber, this.columnNumber]
        };
    }

    private getIconPathWithColor(iconId: string, color: string): vscode.ThemeIcon {
        return new vscode.ThemeIcon(iconId, new vscode.ThemeColor(color));
    }
}