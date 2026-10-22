/// <summary>
/// Entry point of the FPL language server (<c>FplLS</c>).
/// Configures the dependency injection container, registers the LSP handlers
/// (text document sync, completion, formatting, go-to-definition, and find-references),
/// registers the custom requests <c>getTreeData</c> and <c>getWebviewData</c>,
/// and starts the server over standard input and output.
/// </summary>
/// <remarks>
/// The custom requests wait until the interpreter has finished evaluating (<c>heap.IsEvaluating</c>)
/// before returning the symbol table or the valid-statement data as JSON.
/// </remarks>
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
open Fpl3LanguageServer.Buffers.SettingsStore
open Fpl3LanguageServer.ServicesDiagnostics.Diags
open Fpl3LanguageServer.ServiceAutoCompletion.Handler
open Fpl3LanguageServer.ServiceFormatting.Handler
open Fpl3LanguageServer.ServiceGoToDefinition.Handler
open Fpl3LanguageServer.ServiceFindReferences.Handler

/// <summary>
/// Registers the singleton services used by the language server handlers and the buffer and diagnostics infrastructure.
/// </summary>
/// <param name="services">The service collection provided by the OmniSharp language server builder.</param>
let private configureServices (services: IServiceCollection) =
    services.AddSingleton<BufferManager>() |> ignore
    services.AddSingleton<DiagnosticsHandler>() |> ignore
    services.AddSingleton<CompletionHandler>() |> ignore
    services.AddSingleton<SettingsStore>() |> ignore
    services.AddSingleton<FormattingHandler>() |> ignore
    services.AddSingleton<DefinitionHandler>() |> ignore
    services.AddSingleton<ReferencesHandler>() |> ignore

/// <summary>
/// Starts the FPL language server and blocks until the client disconnects.
/// </summary>
/// <param name="_">Command-line arguments (not used).</param>
/// <returns>The process exit code; <c>0</c> on normal shutdown.</returns>
/// <remarks>
/// The server reads from standard input and writes to standard output. The <c>OnInitialize</c> callback
/// subscribes to <c>BufferUpdated</c> so that diagnostics are published whenever a document changes.
/// </remarks>
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
                        .WithHandler<DefinitionHandler>()
                        .WithHandler<ReferencesHandler>()
                        // custom request: symbol table as JSON for the VS Code tree view
                        .OnRequest<JToken, string>("getTreeData", (fun _request _cancellationToken ->
                            while heap.IsEvaluating do ()
                            Task.FromResult(heap.SymbolTable.ToJson())))
                        // custom request: valid statements as JSON for the VS Code webview
                        .OnRequest<JToken, string>("getWebviewData", (fun _request _cancellationToken ->
                            while heap.IsEvaluating do ()
                            Task.FromResult(heap.ValidStmtStore.ToJson())))
                        // publish diagnostics for every buffer update once the server is initialized
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
