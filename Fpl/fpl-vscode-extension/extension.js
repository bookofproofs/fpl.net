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
	// Use the console to output diagnostic information (console.log) and errors (console.error)
	// This line of code will only be executed once when your extension is activated

    // check the platform and architecture
    let platform = process.platform;
    let arch = process.arch;
    var timestamp = new Date().toISOString(); console.log(timestamp + ": running on "+ platform + "/" + arch);


    const path = require('path');
    let serverDllRelative = path.join(__dirname, 'dotnet-runtimes', 'FplLsDll', 'FplLS.dll');
    
    let serverExeRelative = path.join(__dirname, 'dotnet-runtimes', platform, arch, 'dotnet');
    
    timestamp = new Date().toISOString(); console.log(timestamp + ": trying to start FPL Language Server");

    let serverOptions = {
        run: { command: serverExeRelative, args: [serverDllRelative] },
        debug: { command: serverExeRelative, args: [serverDllRelative] }
    };
    
    let clientOptions = {
        documentSelector: [{ scheme: 'file', language: 'fpl' }]
    };

    let client = new LanguageClient(
        'fpl-vscode-extension',
        'Formal Proving Language',
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

    var timestamp = new Date().toISOString(); console.log(timestamp + ': "Formal Proving Language" almost ready, enjoy!');

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

