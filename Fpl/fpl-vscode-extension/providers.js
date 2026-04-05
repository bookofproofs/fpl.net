'use strict';

const vscode = require('vscode');
const { MyTreeItem, ValidStmtItem } = require('./treeItems');
const utils = require('./utils');

function createFplTheoriesProvider(client) {
    class FplTheoriesProvider {
        constructor() {
            this._onDidChangeTreeData = new vscode.EventEmitter();
            this.onDidChangeTreeData = this._onDidChangeTreeData.event;
        }
        refresh() {
            this._onDidChangeTreeData.fire();
        }
        getTreeItem(element) { return element; }
        getChildren(element) {
            if (!element) {
                return client.sendRequest('getTreeData', {}).then(json => {
                    try {
                        let treeData = JSON.parse(json);
                        utils.log2Console(json.substring(0, 1500), true);
                        return this.parseScope(treeData.Scope);
                    } catch (err) {
                        utils.log2Console('Failed to parse tree data: ' + err + ' raw:' + (json ? json.substring(0, 1500) : 'null'), true);
                        return [];
                    }
                }).catch(error => {
                    utils.log2Console('Failed to get tree data ' + error, true);
                    return [];
                });
            } else if (element.isVirtual) {
                return Promise.resolve(this.parseScope(element.scope));
            } else {
                let children = [];
                if (element.scope && element.scope.length > 0) children.push(...this.parseScope(element.scope));
                if (element.arglist && element.arglist.length > 0) children.push(...this.parseArgList(element.arglist));
                if (element.valuelist && element.valuelist.length > 0) children.push(...this.parseValueList(element.valuelist));
                return Promise.resolve(children);
            }
        }
        parseScope(scope) {
            return scope.map(item => new MyTreeItem(item.Type, 1, item.Name, item.Line, item.Column, item.FilePath, item.FplValueType, item.FplValueRepr, item.FplRefersTo, item.Scope, item.ArgList));
        }
        parseArgList(arglist) {
            return arglist.map(item => new MyTreeItem(item.Type, 2, item.Name, item.Line, item.Column, item.FilePath, item.FplValueType, item.FplValueRepr, item.FplRefersTo, item.Scope, item.ArgList));
        }
        parseValueList(valueList) {
            return valueList.map(item => new MyTreeItem(item.Type, 3, item.Name, item.Line, item.Column, item.FilePath, item.FplValueType, item.FplValueRepr, item.FplRefersTo, item.Scope, item.ArgList));
        }
    }
    return new FplTheoriesProvider();
}

function createValidStmtsProvider(client) {
    class ValidStmtsProvider {
        constructor() {
            this._onDidChangeTreeData = new vscode.EventEmitter();
            this.onDidChangeTreeData = this._onDidChangeTreeData.event;
        }
        refresh() { this._onDidChangeTreeData.fire(); }
        getTreeItem(element) { return element; }
        getChildren(element) {
            if (!element) {
                return client.sendRequest('getValidStmts', {}).then(json => {
                    try {
                        let groups = JSON.parse(json);
                        let roots = [];
                        for (let key in groups) {
                            if (Object.prototype.hasOwnProperty.call(groups, key)) {
                                const arr = Array.isArray(groups[key]) ? groups[key] : [];
                                const children = arr.map(obj => {
                                    let label = '';
                                    if (obj && typeof obj === 'object') {
                                        if (typeof obj.statementExpression === 'string') {
                                            label = obj.statementExpression;
                                        } else {
                                            label = JSON.stringify(obj);
                                        }
                                    } else {
                                        label = String(obj);
                                    }
                                    return new ValidStmtItem(label, vscode.TreeItemCollapsibleState.None, [], obj);
                                });
                                const label = `${key} (${children.length})`;
                                roots.push(new ValidStmtItem(label, vscode.TreeItemCollapsibleState.Collapsed, children));
                            }
                        }
                        return roots;
                    } catch (err) {
                        utils.log2Console('Failed to parse valid statements: ' + err + ' raw:' + (json ? json.substring(0, 1500) : 'null'), true);
                        return [];
                    }
                }).catch(error => {
                    utils.log2Console('Failed to get valid statements ' + error, true);
                    return [];
                });
            } else {
                return Promise.resolve(element.children || []);
            }
        }
    }
    return new ValidStmtsProvider();
}

module.exports = {
    createFplTheoriesProvider,
    createValidStmtsProvider
};