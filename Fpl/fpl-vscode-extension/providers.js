// Defines tree view providers for the extension

'use strict';

const vscode = require('vscode');
const { MyTreeItem } = require('./treeItems');
const utils = require('./utils');

function createFplTheoriesProvider(client) {
    class FplTheoriesProvider {
        constructor() {
            this._onDidChangeTreeData = new vscode.EventEmitter();
            this.onDidChangeTreeData = this._onDidChangeTreeData.event;

            // Cache of root-level MyTreeItem nodes from the last successful fetch.
            this._cachedRoots = [];

            // Set of node ids that the user has explicitly expanded.
            // Persisted across refreshes so subtrees survive a reload.
            this._expandedIds = new Set();
        }

        // Called by the treeView expand/collapse listeners in extension.js.
        markExpanded(nodeId) {
            this._expandedIds.add(nodeId);
        }

        markCollapsed(nodeId) {
            this._expandedIds.delete(nodeId);
        }

        // Applies the remembered expanded state to a freshly built item.
        _applyExpandState(item) {
            if (item.collapsibleState === vscode.TreeItemCollapsibleState.None) {
                return;
            }
            item.collapsibleState = this._expandedIds.has(item.id)
                ? vscode.TreeItemCollapsibleState.Expanded
                : vscode.TreeItemCollapsibleState.Collapsed;
        }

        refresh() {
            client.sendRequest('getTreeData', {}).then(json => {
                try {
                    const treeData = JSON.parse(json);
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

        getTreeItem(element) {
            this._applyExpandState(element);
            return element;
        }

        getChildren(element) {
            if (!element) {
                // Return the cached roots — no server round-trip here.
                return Promise.resolve(this._cachedRoots);
            } else if (element.isVirtual) {
                return Promise.resolve(this.parseScope(element.scope));
            } else {
                const children = [];
                if (element.scope && element.scope.length > 0) children.push(...this.parseScope(element.scope));
                if (element.arglist && element.arglist.length > 0) children.push(...this.parseArgList(element.arglist));
                return Promise.resolve(children);
            }
        }

        parseScope(scope) {
            return scope.map(item => {
                const treeItem = new MyTreeItem(item.Type, 1, item.Name, item.Line, item.Column, item.FilePath, item.FplValueType, item.FplValueRepr, item.FplRefersTo, item.Scope, item.ArgList);
                this._applyExpandState(treeItem);
                return treeItem;
            });
        }

        parseArgList(arglist) {
            return arglist.map(item => {
                const treeItem = new MyTreeItem(item.Type, 2, item.Name, item.Line, item.Column, item.FilePath, item.FplValueType, item.FplValueRepr, item.FplRefersTo, item.Scope, item.ArgList);
                this._applyExpandState(treeItem);
                return treeItem;
            });
        }
    }

    return new FplTheoriesProvider();
}


module.exports = {
    createFplTheoriesProvider
}
