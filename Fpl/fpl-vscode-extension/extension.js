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
        console.log('\u001b[' + 31 + 'm' + newMessage + '\u001b[0m');
    }
    else {
        outputChannel.appendLine("Info: " + newMessage);
        // log in green
        console.log('\u001b[' + 32 + 'm' + newMessage + '\u001b[0m');
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
    const fs = require('fs');
    fs.rmdirSync(path, { recursive:true });
}

/**
 * @param {string} path
 */
function makeDirectory(path) {
    const fs = require('fs');
    fs.mkdir(path, (err) => {
        if (err) { throw err; }
    });
    log2Console("Directory " + path + " created successfully", false);
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
                    
                    if (path.extname(pathToDownloadedFile)=='.gz') {
                        tar.x({
                            file: pathToDownloadedFile,
                            cwd: downloadPath
                        });
                        // remove the tar.gz file
                        deleteFile(pathToDownloadedFile);
                        log2Console('Runtime ' + runtimeName + ' installed successfully', false);
                    }
                    else if (path.extname(pathToDownloadedFile)=='.zip') {
                        const unzipper = require('unzipper');
                        // Extract the downloaded runtime zip file into the same directory and delete it after extraction
                        fs.createReadStream(pathToDownloadedFile)
                            .pipe(unzipper.Extract({ path: downloadPath }))
                            .on('close', () => {
                                // remove the zip file
                                deleteFile(pathToDownloadedFile);
                                log2Console('Runtime ' + runtimeName + ' installed successfully', false);
                            });
                    }
                    else
                    {
                        throw new Error("No decompression algorithm found for downloaded file "+pathToDownloadedFile);
                    }
                    return;
                });
            });
            return;
        });
    }
    catch (error) {
        log2Console(error, true);
        throw error;
    }
}

/**
 * @param {string} runtimeName
 */
function dispatchRuntime(runtimeName) {
    if (runtimeName == 'win32-x64') {
        return ['https://download.visualstudio.microsoft.com/download/pr/d8c23e2d-3942-4fb0-8497-04b9f3d9dd8d/46f2d0088b249ca0f5e3b21e710cab97', 'dotnet-runtime-6.0.21-win-x64.zip'];
    }
    else if (runtimeName == "linux-x64") {
        return ['https://download.visualstudio.microsoft.com/download/pr/25fc0412-b2ff-4868-9920-c087b8a75c55/a95292a725fc37c909c4432c74ecdb43', 'dotnet-runtime-6.0.21-linux-x64.tar.gz'];
    }
    else if (runtimeName == "darwin-x64") {
        return ['https://download.visualstudio.microsoft.com/download/pr/af927c74-8c04-4aac-9597-3b56902a812a/47139a25bbc5e58b24fff42f6af0da7c', 'dotnet-runtime-6.0.21-osx-x64.tar.gz'];
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
    log2Console("trying to start FPL Language Server", false);

    const path = require('path');
    var pathToDotNetExe = path.join(relPathToDotnetRuntime, 'dotnet.exe');
    if (directoryOrFileExists(pathToDotNetExe)) {
        // We have a dotnet.exe runtime for this platform / architecture already in the directory relPathToDotnetRuntime
        return;
    }
    else {
        // try to find the runtime 
        const [fileUrlDir, fileUrlName] = dispatchRuntime(runtimeName);
        // drop the old directory (if any, for instance, the last download did not succeeded)        
        if (directoryOrFileExists(relPathToDotnetRuntime)) {
            removeDirectorySync(relPathToDotnetRuntime);
        }
        // if no exception was thrown, make the download/installation directory, because it now does not exist
        makeDirectory(relPathToDotnetRuntime);
        // and install the runtime there
        installRuntime(runtimeName, relPathToDotnetRuntime, fileUrlDir, fileUrlName);
    }
}

// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
const vscode = require('vscode');

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
        let relPathToserverDll = path.join(__dirname, 'dotnet-runtimes', 'FplLsDll', 'FplLS.dll');
        let relPathToDotnetRuntime = path.join(__dirname, 'dotnet-runtimes', runtimeName);
        let relPathToDotnet = path.join(relPathToDotnetRuntime, 'dotnet');

        const result = acquireDotnetRuntime(runtimeName, relPathToDotnetRuntime);

        let serverOptions = {
            run: { command: relPathToDotnet, args: [relPathToserverDll] },
            debug: { command: relPathToDotnet, args: [relPathToserverDll] }
        };

        let clientOptions = {
            documentSelector: [{ scheme: 'file', language: 'fpl' }]
        };

        let client = new LanguageClient(
            'fpl-vscode-extension',
            'FPL Language Server',
            serverOptions,
            clientOptions
        );
       
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

