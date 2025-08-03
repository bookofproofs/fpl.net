let outputChannel;

/**
 * @param {string} message
 * @param {boolean} isError
 */
function log2Console(message, isError) {
    var timestamp = new Date().toISOString();
    var newMessage = timestamp + ": " + message;
    if (isError) {
        outputChannel.appendLine("Error: " + newMessage);
        // log in red
        console.error(newMessage);
    }
    else {
        outputChannel.appendLine("Info: " + newMessage);
        // log in green
        console.info(newMessage);
    }
}

/**
 * @param {string} path
 */
function directoryOrFileExists(path) {
    const fs = require('fs');
    return fs.existsSync(path);
}


function removeDirectorySync(path) {
    return new Promise((resolve, reject) => {
        try {
            if (directoryOrFileExists(path)) {
                const fs = require('fs');
                fs.rmSync(path, { recursive: true });
            }
            resolve("directory removed");
        }
        catch (err) {
            reject(err);
        }
    });
}

/**
 * @param {string} path
 */
function makeDirectory(path) {
    return new Promise((resolve, reject) => {
        const fs = require('fs');
        fs.mkdir(path, (err) => {
            if (err) { reject(err.message); }
            else {
                log2Console("Directory " + path + " created successfully", false);
                resolve("directory created");
            }
        });
    });
}

/**
 * @param {string} pathToFile
 */
function deleteFile(pathToFile) {
    const fs = require('fs');
    fs.unlink(pathToFile, (err) => {
        if (err) { throw err; }
    });
    log2Console("File " + pathToFile + " deleted successfully", false);
}

/**
 * @param {string} runtimeName
 * @param {string} downloadPath
 * @param {string} fileUrlDir
 * @param {string} fileUrlName
 */
function installRuntime(runtimeName, downloadPath, fileUrlDir, fileUrlName) {
    return new Promise((resolve, reject) => {
        try {
            log2Console('trying to install ' + runtimeName, false);
            const https = require('https');
            const fs = require('fs');
            const path = require('path');
            const tar = require('tar');

            // URL of the file to download
            let fileUrl = fileUrlDir + '/' + fileUrlName;

            // Path to save the downloaded file
            let pathToDownloadedFile = downloadPath + '/' + fileUrlName;

            // Download and extract the runtime
            let file = fs.createWriteStream(pathToDownloadedFile);
            https.get(fileUrl, function (response) {
                response.pipe(file);

                file.on('finish', function () {
                    file.close(() => {
                        log2Console('Runtime ' + runtimeName + ' downloaded successfully', false);

                        if (path.extname(pathToDownloadedFile) == '.gz') {
                            tar.x({
                                file: pathToDownloadedFile,
                                cwd: downloadPath
                            });
                            // remove the tar.gz file
                            deleteFile(pathToDownloadedFile);
                            log2Console('runtime ' + runtimeName + ' installed successfully', false);
                            resolve('runtime ' + runtimeName + ' installed successfully');
                        }
                        else if (path.extname(pathToDownloadedFile) == '.zip') {
                            const AdmZip = require('adm-zip');
                            var zip = new AdmZip(pathToDownloadedFile);
                            zip.extractAllTo(downloadPath, true)
                            deleteFile(pathToDownloadedFile);
                            log2Console('runtime ' + runtimeName + ' installed successfully', false);
                            resolve('runtime ' + runtimeName + ' installed successfully');
                        }
                        else {
                            reject("no decompression algorithm found for downloaded file " + pathToDownloadedFile);
                        }
                        return;
                    });
                });
                return;
            });
        }
        catch (error) {
            log2Console(error.message, true);
            reject(error.message);
        }
    });
}

/**
 * @param {string} runtimeName
 */
function dispatchRuntime(runtimeName) {
    if (runtimeName == 'win32-x64') {
        return ['https://github.com/bookofproofs/fpl.netlib/raw/refs/heads/main', 'dotnet-runtime-8.0.8-win-x64.zip'];
    }
    else if (runtimeName == "linux-x64") {
        return ['https://github.com/bookofproofs/fpl.netlib/raw/refs/heads/main', 'dotnet-runtime-8.0.8-linux-x64.tar.gz'];
    }
    else if (runtimeName == "darwin-x64") {
        return ['https://github.com/bookofproofs/fpl.netlib/raw/refs/heads/main', 'dotnet-runtime-8.0.8-osx-x64.tar.gz'];
    }
    else {
        let errorMsg = "Unfortunately, no runtime found for your system " + runtimeName;
        log2Console(errorMsg, true);
        throw new Error(errorMsg);
    }
}


/**
 * @param {string} runtimeName
 * @param {string} relPathToDotnetRuntime
 */
function acquireDotnetRuntime(runtimeName, relPathToDotnetRuntime) {
    return new Promise((resolve, reject) => {
        let condition;
        const path = require('path');
        var pathToDotNetExe = path.join(relPathToDotnetRuntime, 'dotnet.exe');
        if (directoryOrFileExists(pathToDotNetExe)) {
            // We have a dotnet.exe runtime for this platform / architecture already in the directory relPathToDotnetRuntime
            resolve('dotnet runtime acquired successfully');
        }
        else {
            // try to find the runtime 
            const [fileUrlDir, fileUrlName] = dispatchRuntime(runtimeName);
            const removeDirectoryPromise = removeDirectorySync(relPathToDotnetRuntime);
            // drop the old directory (if any, for instance, the last download did not succeeded)        
            removeDirectoryPromise.then((message) => {
                // if no exception was thrown, create the download/installation directory, because it now does not exist
                const makeDirectoryPromise = makeDirectory(relPathToDotnetRuntime);
                makeDirectoryPromise.then((message) => {
                    // and install the runtime there
                    const installRuntimePromice = installRuntime(runtimeName, relPathToDotnetRuntime, fileUrlDir, fileUrlName);
                    installRuntimePromice.then((message) => {

                        resolve('dotnet runtime acquired properly');
                    }).catch((message) => {
                        reject(message);
                    });
                }).catch((message) => {
                    reject(message);
                });
            }).catch((message) => {
                reject(message);
            });
        }
    });
}

// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
const vscode = require('vscode');


// a map of the types
const typeToIconMap = new Map();
typeToIconMap.set('th','library');
typeToIconMap.set('var','variable');
typeToIconMap.set('*var','bracket-error');
typeToIconMap.set('+var','bracket-dot');
typeToIconMap.set('mpred','symbol-boolean');
typeToIconMap.set('opred','symbol-boolean');
typeToIconMap.set('mfunc','symbol-interface');
typeToIconMap.set('ofunc','symbol-interface');
typeToIconMap.set('ctor','symbol-constructor');
typeToIconMap.set('def cl','symbol-class');
typeToIconMap.set('obj','primitive-square');
typeToIconMap.set('loc','location');
typeToIconMap.set('thm','layout-panel-justify');
typeToIconMap.set('lem','layout-panel-center');
typeToIconMap.set('prop','layout-panel-right');
typeToIconMap.set('cor','layout-sidebar-right');
typeToIconMap.set('prf','testing-passed-icon');
typeToIconMap.set('conj','workspace-unknown');
typeToIconMap.set('ax','layout');
typeToIconMap.set('inf','symbol-structure');
typeToIconMap.set('qtr','circuit-board');
typeToIconMap.set('def pred','symbol-boolean');
typeToIconMap.set('pred','symbol-boolean');
typeToIconMap.set('def func','symbol-interface');
typeToIconMap.set('func','symbol-interface');
typeToIconMap.set('ref','link');
typeToIconMap.set('arg','indent');
typeToIconMap.set('just','kebab-horizontal');
typeToIconMap.set('ainf','kebab-vertical');
typeToIconMap.set('lang','globe');
typeToIconMap.set('trsl','symbol-text');
typeToIconMap.set('map','preview');
typeToIconMap.set('stmt','symbol-event');
typeToIconMap.set('ass','target');
typeToIconMap.set('ext','extensions');
typeToIconMap.set('inst','output');
typeToIconMap.set('ind','symbol-numeric');
typeToIconMap.set('tpl','gear');
typeToIconMap.set('undef','question');




// A custom TreeItem
class MyTreeItem extends vscode.TreeItem {
    constructor(typ, inScope, label, lineNumber, columnNumber, filePath, fplValueType, fplValueRepr, valuelist = [], scope = [], arglist = []) {
        super(label, scope.length > 0 || arglist.length > 0 || valuelist.length > 0 ? vscode.TreeItemCollapsibleState.Collapsed : vscode.TreeItemCollapsibleState.None);
        this.typ = typ;
        this.lineNumber = lineNumber;
        this.columnNumber = columnNumber;
        this.filePath = filePath;
        if (typ == "mpred") 
        {
            this.label = "pred prop " + label;
        }
        else if (typ == "opred") 
        {
            this.label = "opt pred prop " + label;
        }
        else if (typ == "mfunc") 
        {
            this.label = "func prop " + label;
        }
        else if (typ == "ofunc") 
        {
            this.label = "opt func prop " + label;
        }
        else 
        {
            this.label = typ + " " + label;
        }
        // tooltip showing the name, the type and the representation of a node
        this.tooltip = "n: " + label + "\nt: "+ fplValueType + "\nr: " + fplValueRepr;
        this.scope = scope;
        if (this.typ == "th") log2Console(this.label + " " + scope.length, false);
        this.arglist = arglist;
        this.valuelist = valuelist;

        if (inScope == 1) {
            this.iconPath = this.getIconPathWithColor(typeToIconMap.get(typ) || 'default-view-icon', 'textPreformat.foreground');
        }
        else if (inScope == 2) {
            this.iconPath = this.getIconPathWithColor(typeToIconMap.get(typ) || 'default-view-icon', 'focusBorder');
        }
        else {
            this.iconPath = this.getIconPathWithColor(typeToIconMap.get(typ) || 'default-view-icon', 'textSeparator.foreground');
        }

        // Set the command to open the file and navigate to the line number
        this.command = {
            command: 'extension.openFileAtPosition',
            title: 'Open File',
            arguments: [this.filePath, this.lineNumber, this.columnNumber]
        };

    }

    // Method to get the colorized icon
    getIconPathWithColor(iconId, color) {
        return new vscode.ThemeIcon(iconId, new vscode.ThemeColor(color));
    }

    
}


// Define TreeDataProvider
class FplTheoriesProvider {
    constructor() {
        this._onDidChangeTreeData = new vscode.EventEmitter();
        this.onDidChangeTreeData = this._onDidChangeTreeData.event;
    }

    refresh() {
        this._onDidChangeTreeData.fire();
    }

    getTreeItem(element) {
        return element;
    }

    getChildren(element) {
        if (!element) {
            // If no element is passed, return the root nodes of the tree
            return client.sendRequest('getTreeData', {}).then(json => {
                let treeData = JSON.parse(json);
                return this.parseScope(treeData.Scope);
            }).catch(error => {
                log2Console('Failed to get tree data ' + error, true);
                return [];  // Return an empty array on error
            });
        } else if (element.isVirtual) {
            // Handle virtual nodes
            return Promise.resolve(this.parseScope(element.scope));
        } else {
            // Create virtual child elements for Scope and ArgList if they are not empty
            let children = [];
            if (element.scope && element.scope.length > 0) {
                children.push(...this.parseScope(element.scope));
            }
            if (element.arglist && element.arglist.length > 0) {
                children.push(...this.parseArgList(element.arglist));
            }
            if (element.valuelist && element.valuelist.length > 0) {
                children.push(...this.parseValueList(element.valuelist));
            }
            return Promise.resolve(children);
        }
    }

    parseScope(scope) {
        // Convert each item in the scope to a MyTreeItem

        return scope.map(item => new MyTreeItem(item.Type, 1, item.Name, item.Line, item.Column, item.FilePath, item.FplValueType, item.FplValueRepr, item.ValueList, item.Scope, item.ArgList));
    }

    parseArgList(arglist) {
        // Convert each item in the arglist to a MyTreeItem
        return arglist.map(item => new MyTreeItem(item.Type, 2, item.Name, item.Line, item.Column, item.FilePath, item.FplValueType, item.FplValueRepr, item.ValueList, item.Scope, item.ArgList));
    }

    parseValueList(valueList) {
        // Convert each item in the valueList to a MyTreeItem
        return valueList.map(item => new MyTreeItem(item.Type, 3, item.Name, item.Line, item.Column, item.FilePath, item.FplValueType, item.FplValueRepr, item.ValueList, item.Scope, item.ArgList));
    }
}


const { LanguageClient } = require('vscode-languageclient');

let client;

// This method is called when your extension is activated
// The extension is configured to be activated via the onLanguage event in package.json

/**
 * @param {vscode.ExtensionContext} context
 */
function activate(context) {
    outputChannel = vscode.window.createOutputChannel('FPL Log');
    try {

        // Use the console to output diagnostic information (console.log) and errors (console.error)
        // This line of code will only be executed once when your extension is activated

        // check the platform and architecture
        let platform = process.platform;
        let arch = process.arch;
        let runtimeName = platform + '-' + arch;
        log2Console("running on " + runtimeName, false);


        const path = require('path');
        let relPathToServerDll = path.join(__dirname, 'dotnet-runtimes', 'FplLsDll', 'FplLS.dll');
        let relPathToDotnetRuntime = path.join(__dirname, 'dotnet-runtimes', runtimeName);
        let relPathToDotnet = path.join(relPathToDotnetRuntime, 'dotnet');

        const acquireDotNetRuntimePromise = acquireDotnetRuntime(runtimeName, relPathToDotnetRuntime);

        acquireDotNetRuntimePromise.then(() => {
            let serverOptions = {
                run: { command: relPathToDotnet, args: [relPathToServerDll] },
                debug: { command: relPathToDotnet, args: [relPathToServerDll] }
            };

            let clientOptions = {
                documentSelector: [{ scheme: 'file', language: 'fpl' }]
            };

            client = new LanguageClient(
                'fpl-vscode-extension',
                'FPL Language Server',
                serverOptions,
                clientOptions
            );

            // Create an instance of your TreeDataProvider
            const fplTheoriesProvider = new FplTheoriesProvider();

            // Register TreeDataProvider
            vscode.window.registerTreeDataProvider('fplTheories', fplTheoriesProvider);

            // refresh FPL Theories Explorer on document open event  
            vscode.workspace.onDidOpenTextDocument((document) => {
                log2Console("onDidOpenTextDocument", false);
                if (document.languageId === 'fpl') {
                    fplTheoriesProvider.refresh();
                }
            });

            // refresh FPL Theories Explorer on active text editor changes   
            vscode.window.onDidChangeActiveTextEditor((editor) => {
                log2Console("onDidChangeActiveTextEditor", false);
                if (editor && editor.document.languageId === 'fpl') {
                    fplTheoriesProvider.refresh();
                }
            });

            // refresh FPL Theories Explorer on document changes  
            vscode.workspace.onDidChangeTextDocument((event) => {
                if (event.document.languageId === 'fpl') {
                    fplTheoriesProvider.refresh();
                }
            });

            let config = vscode.workspace.getConfiguration('fplExtension');

            // Convert the configuration to a JSON string.
            let configJson = JSON.stringify(config, null, 2);

            // Write the configuration to a file.
            let fs = require('fs');
            let relPathToConfig = path.join(__dirname, 'dotnet-runtimes', 'FplLsDll', 'vsfplconfig.json');
            fs.writeFile(relPathToConfig, configJson, (err) => {
                if (err) {
                    log2Console('Error writing file:' + err.message, true);
                }
            });

            let disposableClient = client.start();

            // The command has been defined in the package.json file
            // Now provide the implementation of the command with  registerCommand
            // The commandId parameter must match the command field in package.json
            let disposableCommand = vscode.commands.registerCommand('fpl-vscode-extension.helloWorld', function () {

                // The code you place here will be executed every time your command is executed
                // Display a message box to the user
                vscode.window.showInformationMessage('Hello World from "Formal Proving Language"!');
            });

            // initial tree view refresh if there is already an active text editor
            if (vscode.window.activeTextEditor && vscode.window.activeTextEditor.document.languageId === 'fpl') {
                log2Console("initial treeview refresh", false);
                fplTheoriesProvider.refresh();
            }

            // Register the command
            let disposableCommand2 = vscode.commands.registerCommand('extension.openFileAtPosition', (filePath, lineNumber, columnNumber) => {
                const openPath = vscode.Uri.file(filePath);
                vscode.workspace.openTextDocument(openPath).then(doc => {
                    vscode.window.showTextDocument(doc).then(editor => {
                        const position = new vscode.Position(lineNumber-1, columnNumber-1);
                        const range = new vscode.Range(position, position);
                        editor.selection = new vscode.Selection(position, position);
                        editor.revealRange(range);
                    });
                });
            });

            context.subscriptions.push(disposableClient);
            context.subscriptions.push(disposableCommand);
            context.subscriptions.push(disposableCommand2);

            log2Console('Launching "Formal Proving Language", enjoy!', false);
        });

    }
    catch (error) {
        let errorMsg = 'Installing "Formal Proving Language" failed :-(, report issue on https://github.com/bookofproofs/fpl.net';
        log2Console(errorMsg, true);
        throw new Error(errorMsg);
    }

}
// This method is called when your extension is deactivated
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}

module.exports = {
    activate,
    deactivate
}

