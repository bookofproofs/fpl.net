(*
MIT License

Copyright (c) 2018 Martin Björkström

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.  
*)
module FplLS.Program

open System
open System.Threading.Tasks
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Newtonsoft.Json.Linq
open OmniSharp.Extensions.LanguageServer.Server
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.SymbolTable.Storage.Heap
open FplLsLib.Buffers.BuffMgr
open FplLsLib.Buffers.DocSync
open FplLsLib.ServicesDiagnostics.Diags
open FplLsLib.ServiceAutoCompletion.Handler

let private configureServices (services: IServiceCollection) =
    services.AddSingleton<BufferManager>() |> ignore
    services.AddSingleton<DiagnosticsHandler>() |> ignore
    services.AddSingleton<CompletionHandler>() |> ignore

[<EntryPoint>]
let main _ =
    let task =
        task {
            let! server =
                LanguageServer.From(fun options ->
                    options
                        .WithInput(Console.OpenStandardInput())
                        .WithOutput(Console.OpenStandardOutput())
                        .WithLoggerFactory(new LoggerFactory())
                        .AddDefaultLoggingProvider()
                        .WithServices(Action<IServiceCollection>(configureServices))
                        .WithHandler<TextDocumentSyncHandler>()
                        .WithHandler<CompletionHandler>()
                        .OnRequest<JToken, string>("getTreeData", (fun _request _cancellationToken ->
                            while heap.IsEvaluating do ()
                            Task.FromResult(heap.SymbolTable.ToJson())))
                        .OnRequest<JToken, string>("getWebviewData", (fun _request _cancellationToken ->
                            while heap.IsEvaluating do ()
                            Task.FromResult(heap.ValidStmtStore.ToJson())))
                        .OnInitialize(fun s _ _ ->
                            match s with
                            | :? LanguageServer as languageServer ->
                                let serviceProvider = languageServer.Services
                                let bufferManager = serviceProvider.GetService<BufferManager>()
                                let diagnosticsHandler = serviceProvider.GetService<DiagnosticsHandler>()

                                bufferManager.BufferUpdated.Add(fun x ->
                                    diagnosticsHandler.PublishDiagnostics(
                                        PathEquivalentUri(x.Uri.AbsoluteUri),
                                        bufferManager.GetBuffer(x.Uri)))

                                Task.CompletedTask
                            | _ -> raise (Exception("Failed to cast s to LanguageServer")))
                    |> ignore)
                |> Async.AwaitTask

            do! server.WaitForExit |> Async.AwaitTask
        }
    task.Wait()
    0
