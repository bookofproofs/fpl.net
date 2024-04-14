@echo off
set "source_folder=.\Fpl\FplLS\bin\Release\net8.0"
set "destination_folder=.\Fpl\fpl-vscode-extension\dotnet-runtimes\FplLsDll"

REM Delete all files in the destination folder
del /Q "%destination_folder%\*.*"

REM Copy all files from the source folder to the destination folder
xcopy /Y "%source_folder%\*.*" "%destination_folder%\"

echo All files have been copied from %source_folder% to %destination_folder%.