#!/bin/bash
source_folder="./Fpl/FplLS/bin/Release/net8.0"
destination_folder="./Fpl/fpl-vscode-extension/dotnet-runtimes/FplLsDll"

# Delete all files in the destination folder
rm -rf "$destination_folder/*"

# Copy all files from the source folder to the destination folder
cp -r "$source_folder/"* "$destination_folder/"

echo "All files have been copied from $source_folder to $destination_folder."