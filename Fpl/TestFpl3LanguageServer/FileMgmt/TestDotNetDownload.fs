namespace TestFpl3LanguageServer.FileMgmt

open System
open System.IO
open System.Net.Http
open System.Threading.Tasks
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl2Interpreter.Helpers.Debug

[<TestClass>]
type TestDotNetDownload() =

    [<DataRow("https://github.com/bookofproofs/fpl.netlib/raw/refs/heads/main/dotnet-runtime-8.0.8-win-x64.zip", "winfile.zip")>]
    [<DataRow("https://github.com/bookofproofs/fpl.netlib/raw/refs/heads/main/dotnet-runtime-8.0.8-linux-x64.tar.gz", "linuxfile.gz")>]
    [<DataRow("https://github.com/bookofproofs/fpl.netlib/raw/refs/heads/main/dotnet-runtime-8.0.8-osx-x64.tar.gz", "osxfile.gz")>]
    [<TestMethod>]
    member this.TestFileDownload(url: string, localFilePath: string) =
        task {
            if not offlineWatcher.OfflineMode then
                use client = new HttpClient()
                client.Timeout <- TimeSpan.FromSeconds(60.0)
                try
                    let! response = client.GetAsync(url)

                    if response.IsSuccessStatusCode then
                        let fileStream = File.Create(localFilePath)
                        do! response.Content.CopyToAsync(fileStream)
                        fileStream.Close()

                        // If the file download is successful, the file should exist at the local file path
                        Assert.IsTrue(File.Exists(localFilePath))

                        // Clean up the downloaded file
                        //File.Delete(localFilePath)
                    else
                        Assert.Fail("File could not be downloaded. Status code: " + response.StatusCode.ToString())
                with
                | :? HttpRequestException as ex ->
                    // If a HttpRequestException is thrown, the file could not be downloaded
                    Assert.Fail($"File could not be downloaded. {ex.Message}")
        } :> Task
