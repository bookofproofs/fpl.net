// Defines tree view providers for the extension

'use strict';

import * as vscode from 'vscode';
import { LanguageClient } from 'vscode-languageclient/node';
import { MyTreeItem } from './treeItems';
import * as utils from './utils';

interface FplScopeItemJson {
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

interface FplTreeDataJson {
    Scope: FplScopeItemJson[];
}

class FplTheoriesProvider implements vscode.TreeDataProvider<MyTreeItem> {
    private readonly _onDidChangeTreeData: vscode.EventEmitter<void> = new vscode.EventEmitter<void>();
    public readonly onDidChangeTreeData: vscode.Event<void> = this._onDidChangeTreeData.event;

    // Cache of root-level MyTreeItem nodes from the last successful fetch.
    private _cachedRoots: MyTreeItem[] = [];

    // Set of node ids that the user has explicitly expanded.
    // Persisted across refreshes so subtrees survive a reload.
    private readonly _expandedIds: Set<string> = new Set();

    constructor(private readonly client: LanguageClient) {}

    // Called by the treeView expand/collapse listeners in extension.ts.
    public markExpanded(nodeId: string): void {
        this._expandedIds.add(nodeId);
    }

    public markCollapsed(nodeId: string): void {
        this._expandedIds.delete(nodeId);
    }

    // Applies the remembered expanded state to a freshly built item.
    private _applyExpandState(item: MyTreeItem): void {
        if (item.collapsibleState === vscode.TreeItemCollapsibleState.None) {
            return;
        }
        item.collapsibleState = item.id !== undefined && this._expandedIds.has(item.id)
            ? vscode.TreeItemCollapsibleState.Expanded
            : vscode.TreeItemCollapsibleState.Collapsed;
    }

    public refresh(): void {
        this.client.sendRequest<string>('getTreeData', {}).then(json => {
            try {
                const treeData: FplTreeDataJson = JSON.parse(json);
                this._cachedRoots = this.parseScope(treeData.Scope);
            } catch (err) {
                utils.log2Console('Failed to parse tree data: ' + err + ' raw:' + (json ? json.substring(0, 1500) : 'null'), true);
                this._cachedRoots = [];
            }
            this._onDidChangeTreeData.fire();
        }).catch(error => {
            utils.log2Console('Failed to get tree data ' + error, true);
            this._cachedRoots = [];
            this._onDidChangeTreeData.fire();
        });
    }

    public getTreeItem(element: MyTreeItem): vscode.TreeItem {
        this._applyExpandState(element);
        return element;
    }

    public getChildren(element?: MyTreeItem): Thenable<MyTreeItem[]> {
        if (!element) {
            // Return the cached roots — no server round-trip here.
            return Promise.resolve(this._cachedRoots);
        } else if (element.isVirtual) {
            return Promise.resolve(this.parseScope(element.scope as unknown as FplScopeItemJson[]));
        } else {
            const children: MyTreeItem[] = [];
            if (element.scope && element.scope.length > 0) children.push(...this.parseScope(element.scope as unknown as FplScopeItemJson[]));
            if (element.arglist && element.arglist.length > 0) children.push(...this.parseArgList(element.arglist as unknown as FplScopeItemJson[]));
            return Promise.resolve(children);
        }
    }

    private parseScope(scope: FplScopeItemJson[]): MyTreeItem[] {
        return scope.map(item => {
            const treeItem = new MyTreeItem(item.Type, 1, item.Name, item.Line, item.Column, item.FilePath, item.FplValueType, item.FplValueRepr, item.FplRefersTo, item.Scope as unknown as MyTreeItem[], item.ArgList as unknown as MyTreeItem[]);
            this._applyExpandState(treeItem);
            return treeItem;
        });
    }

    private parseArgList(arglist: FplScopeItemJson[]): MyTreeItem[] {
        return arglist.map(item => {
            const treeItem = new MyTreeItem(item.Type, 2, item.Name, item.Line, item.Column, item.FilePath, item.FplValueType, item.FplValueRepr, item.FplRefersTo, item.Scope as unknown as MyTreeItem[], item.ArgList as unknown as MyTreeItem[]);
            this._applyExpandState(treeItem);
            return treeItem;
        });
    }
}

export function createFplTheoriesProvider(client: LanguageClient): FplTheoriesProvider {
    return new FplTheoriesProvider(client);
}