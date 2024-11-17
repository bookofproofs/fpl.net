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

// A custom TreeItem
class MyTreeItem extends vscode.TreeItem {
    constructor(label, scope = [], valueList = []) {
        super(label, scope.length > 0 || valueList.length > 0 ? vscode.TreeItemCollapsibleState.Collapsed : vscode.TreeItemCollapsibleState.None);
        this.label = label;
        this.scope = scope;
        this.valueList = valueList;

        this.iconPath = this.getIconPath(label);
    }

    getIconPath(label) {
        return new vscode.ThemeIcon('file');
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
            // Create virtual child elements for Scope and ValueList if they are not empty
            let children = [];
            if (element.scope && element.scope.length > 0) {
                children.push(...this.parseScope(element.scope));
            }
            if (element.valueList && element.valueList.length > 0) {
                children.push(...this.parseValueList(element.valueList));
            }
            return Promise.resolve(children);
        }
    }

    parseScope(scope) {
        // Convert each item in the scope to a MyTreeItem
        return scope.map(item => new MyTreeItem("#" + item.Type + ": " + item.Name, item.Scope, item.ValueList));
    }

    parseValueList(valueList) {
        // Convert each item in the valueList to a MyTreeItem
        return valueList.map(item => new MyTreeItem(item.Type + ": " + item.Name, item.Scope, item.ValueList));
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
                if (document.languageId === 'fpl') {
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
                    console.error('Error writing file:', err);
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

            context.subscriptions.push(disposableClient);
            context.subscriptions.push(disposableCommand);

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

