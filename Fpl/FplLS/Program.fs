
open System
open System.Threading.Tasks
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Newtonsoft.Json.Linq
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open OmniSharp.Extensions.LanguageServer.Server
open Fpl0Base.Errors.Diagnostics
open Fpl2Interpreter.SymbolTable.Storage.Heap
open Fpl3LanguageServer.Buffers.BuffMgr
open Fpl3LanguageServer.Buffers.DocSync
open Fpl3LanguageServer.Buffers.SettingsStore
open Fpl3LanguageServer.Buffers.Logging
open Fpl3LanguageServer.ServicesDiagnostics.Diags
open Fpl3LanguageServer.ServiceAutoCompletion.Handler
open Fpl3LanguageServer.ServiceFormatting.Handler
open Fpl3LanguageServer.ServiceFormatting.ConfigurationHandler
open Fpl3LanguageServer.ServiceFormatting.SettingsPull

let private configureServices (services: IServiceCollection) =
    services.AddSingleton<BufferManager>() |> ignore
    services.AddSingleton<DiagnosticsHandler>() |> ignore
    services.AddSingleton<CompletionHandler>() |> ignore
    services.AddSingleton<SettingsStore>() |> ignore
    services.AddSingleton<FormattingHandler>() |> ignore
    services.AddSingleton<FormattingConfigurationHandler>() |> ignore

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
                        .WithHandler<FormattingHandler>()
                        .WithHandler<FormattingConfigurationHandler>()
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
                                let settingsStore = serviceProvider.GetService<SettingsStore>()

                                bufferManager.BufferUpdated.Add(fun x ->
                                    diagnosticsHandler.PublishDiagnostics(
                                        PathEquivalentUri(x.Uri.AbsoluteUri),
                                        bufferManager.GetBuffer(x.Uri)))

                                // Best-effort initial pull; FormattingHandler re-pulls on every
                                // request regardless, so this just seeds SettingsStore early for
                                // any other consumer.
                                try
                                    settingsStore.Update(pullCurrentOptions languageServer.Configuration)
                                with ex ->
                                    logException languageServer ex "OnInitialize.Configuration"

                                Task.CompletedTask
                            | _ -> raise (Exception("Failed to cast s to LanguageServer")))
                    |> ignore)
                |> Async.AwaitTask

            do! server.WaitForExit |> Async.AwaitTask
        }
    task.Wait()
    0
