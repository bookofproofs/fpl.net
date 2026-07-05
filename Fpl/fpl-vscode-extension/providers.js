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
                return Promise.resolve(children);
            }
        }
        parseScope(scope) {
            return scope.map(item => new MyTreeItem(item.Type, 1, item.Name, item.Line, item.Column, item.FilePath, item.FplValueType, item.FplValueRepr, item.FplRefersTo, item.Scope, item.ArgList));
        }
        parseArgList(arglist) {
            return arglist.map(item => new MyTreeItem(item.Type, 2, item.Name, item.Line, item.Column, item.FilePath, item.FplValueType, item.FplValueRepr, item.FplRefersTo, item.Scope, item.ArgList));
        }
    }
    return new FplTheoriesProvider();
}


module.exports = {
    createFplTheoriesProvider
}