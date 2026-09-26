module FplLS.Program

open System
open System.Threading.Tasks
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Newtonsoft.Json.Linq
open OmniSharp.Extensions.LanguageServer.Server
open Fpl0Base.Errors.Diagnostics
open Fpl2Interpreter.SymbolTable.Storage.Heap
open Fpl3LanguageServer.Buffers.BuffMgr
open Fpl3LanguageServer.Buffers.DocSync
open Fpl3LanguageServer.ServicesDiagnostics.Diags
open Fpl3LanguageServer.ServiceAutoCompletion.Handler

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
