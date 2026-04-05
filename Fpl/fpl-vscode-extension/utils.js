'use strict';

const fs = require('fs');
const https = require('https');
const urlModule = require('url');
const path = require('path');
const tar = require('tar');
const AdmZip = require('adm-zip');

let outputChannel = null;

function init(outChannel) {
    outputChannel = outChannel;
}

function log2Console(message, isError) {
    var timestamp = new Date().toISOString();
    var newMessage = timestamp + ': ' + message;
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

function directoryOrFileExists(p) {
    return fs.existsSync(p);
}

function removeDirectorySync(p) {
    return new Promise((resolve, reject) => {
        try {
            if (directoryOrFileExists(p)) {
                fs.rmSync(p, { recursive: true });
            }
            resolve('directory removed');
        } catch (err) {
            reject(err);
        }
    });
}

function makeDirectory(p) {
    return new Promise((resolve, reject) => {
        fs.mkdir(p, (err) => {
            if (err) { reject(err.message); }
            else {
                log2Console('Directory ' + p + ' created successfully', false);
                resolve('directory created');
            }
        });
    });
}

function deleteFile(pathToFile) {
    fs.unlink(pathToFile, (err) => {
        if (err) { throw err; }
    });
    log2Console('File ' + pathToFile + ' deleted successfully', false);
}

function downloadFile(url, dest, maxRedirects = 5) {
    return new Promise((resolve, reject) => {
        const doRequest = (reqUrl, redirectsLeft) => {
            const file = fs.createWriteStream(dest);
            https.get(reqUrl, (response) => {
                if (response.statusCode >= 300 && response.statusCode < 400 && response.headers.location) {
                    if (redirectsLeft === 0) {
                        reject(new Error('Too many redirects'));
                        return;
                    }
                    file.close(() => {});
                    const redirectUrl = urlModule.resolve(reqUrl, response.headers.location);
                    doRequest(redirectUrl, redirectsLeft - 1);
                    return;
                }
                if (response.statusCode !== 200) {
                    reject(new Error(`Failed to get '${reqUrl}' (${response.statusCode})`));
                    response.resume();
                    return;
                }
                response.pipe(file);
                file.on('finish', () => file.close(resolve));
            }).on('error', (err) => {
                fs.unlink(dest, () => reject(err));
            });
            file.on('error', (err) => {
                fs.unlink(dest, () => reject(err));
            });
        };
        doRequest(url, maxRedirects);
    });
}

function dispatchRuntime(runtimeName) {
    if (runtimeName === 'win32-x64') {
        return ['https://github.com/bookofproofs/fpl.netlib/raw/refs/heads/main', 'dotnet-runtime-8.0.8-win-x64.zip'];
    } else if (runtimeName === 'linux-x64') {
        return ['https://github.com/bookofproofs/fpl.netlib/raw/refs/heads/main', 'dotnet-runtime-8.0.8-linux-x64.tar.gz'];
    } else if (runtimeName === 'darwin-x64') {
        return ['https://github.com/bookofproofs/fpl.netlib/raw/refs/heads/main', 'dotnet-runtime-8.0.8-osx-x64.tar.gz'];
    } else {
        const errorMsg = 'Unfortunately, no runtime found for your system ' + runtimeName;
        log2Console(errorMsg, true);
        throw new Error(errorMsg);
    }
}

function installRuntime(runtimeName, downloadPath, fileUrlDir, fileUrlName) {
    return new Promise((resolve, reject) => {
        log2Console('trying to download ' + runtimeName + ' to ' + downloadPath + ' from ' + fileUrlDir + '/' + fileUrlName, false);
        const fileUrl = fileUrlDir + '/' + fileUrlName;
        const pathToDownloadedFile = downloadPath + '/' + fileUrlName;
        downloadFile(fileUrl, pathToDownloadedFile)
            .then(() => {
                log2Console('Runtime ' + runtimeName + ' downloaded successfully', false);
                if (path.extname(pathToDownloadedFile) === '.gz') {
                    log2Console('Trying to unpack using tar: ' + pathToDownloadedFile, false);
                    tar.x({
                        file: pathToDownloadedFile,
                        cwd: downloadPath
                    });
                    deleteFile(pathToDownloadedFile);
                    log2Console('runtime ' + runtimeName + ' installed successfully', false);
                    resolve('runtime ' + runtimeName + ' installed successfully');
                } else if (path.extname(pathToDownloadedFile) === '.zip') {
                    log2Console('Trying to unpack using adm-zip: ' + pathToDownloadedFile, false);
                    const zip = new AdmZip(pathToDownloadedFile);
                    zip.extractAllTo(downloadPath, true);
                    deleteFile(pathToDownloadedFile);
                    log2Console('runtime ' + runtimeName + ' installed successfully', false);
                    resolve('runtime ' + runtimeName + ' installed successfully');
                } else {
                    reject('no decompression algorithm found for downloaded file ' + pathToDownloadedFile);
                }
            })
            .catch(err => {
                log2Console('Automatic download failed:' + err, true);
                log2Console('To resolve this: 1) Try manual download. 2) Unpack to the folder. 3) Restart VS Code.', false);
                reject(err);
            });
    });
}

function acquireDotnetRuntime(runtimeName, relPathToDotnetRuntime) {
    return new Promise((resolve, reject) => {
        const pathToDotNetExe = path.join(relPathToDotnetRuntime, 'dotnet.exe');
        if (directoryOrFileExists(pathToDotNetExe)) {
            resolve('dotnet runtime acquired successfully');
        } else {
            const [fileUrlDir, fileUrlName] = dispatchRuntime(runtimeName);
            removeDirectorySync(relPathToDotnetRuntime).then(() => {
                return makeDirectory(relPathToDotnetRuntime);
            }).then(() => {
                return installRuntime(runtimeName, relPathToDotnetRuntime, fileUrlDir, fileUrlName);
            }).then(msg => {
                resolve(msg);
            }).catch(err => {
                reject(err);
            });
        }
    });
}

module.exports = {
    init,
    log2Console,
    directoryOrFileExists,
    removeDirectorySync,
    makeDirectory,
    deleteFile,
    downloadFile,
    installRuntime,
    dispatchRuntime,
    acquireDotnetRuntime
};