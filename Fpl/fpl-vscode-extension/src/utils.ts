// centralizes logging, file operations, and runtime installation

'use strict';

import * as fs from 'fs';
import * as vscode from 'vscode';

let outputChannel: vscode.OutputChannel | undefined;

export function init(outChannel: vscode.OutputChannel | undefined): void {
    outputChannel = outChannel;
}

export function log2Console(message: string, isError: boolean): void {
    const timestamp = new Date().toISOString();
    const newMessage = timestamp + ': ' + message;
    if (outputChannel) {
        if (isError) {
            outputChannel.appendLine('Error: ' + newMessage);
            console.error(newMessage);
        } else {
            outputChannel.appendLine('Info: ' + newMessage);
            console.info(newMessage);
        }
    } else {
        if (isError) console.error(newMessage);
        else console.info(newMessage);
    }
}

export function directoryOrFileExists(p: string): boolean {
    return fs.existsSync(p);
}

