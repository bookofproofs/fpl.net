module FplLsLib.ServicesDiagnostics.Diags

open System
open System.Collections.Generic
open System.Text
open OmniSharp.Extensions.LanguageServer.Protocol.Server
open OmniSharp.Extensions.LanguageServer.Protocol.Window
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.Main
open FplLsLib.Buffers.TextPos


/// <summary>
/// An alias for the type OmniSharp.Extensions.LanguageServer.Protocol.Models.Diagnostic
/// </summary>
type UODiagnostic = OmniSharp.Extensions.LanguageServer.Protocol.Models.Diagnostic

/// <summary>
/// An alias for the type OmniSharp.Extensions.LanguageServer.Protocol.Models.DiagnosticSeverity
/// </summary>
type UODiagnosticSeverity = OmniSharp.Extensions.LanguageServer.Protocol.Models.DiagnosticSeverity

/// <summary>
/// An alias for the type OmniSharp.Extensions.LanguageServer.Protocol.Document.PublishDiagnosticsExtensions
/// </summary>
type UOPublishDiagnosticsExtensions = OmniSharp.Extensions.LanguageServer.Protocol.Document.PublishDiagnosticsExtensions


/// <summary>
/// An alias for the type OmniSharp.Extensions.LanguageServer.Protocol.Models.PublishDiagnosticsParams
/// </summary>
type UOPublishDiagnosticsParams = OmniSharp.Extensions.LanguageServer.Protocol.Models.PublishDiagnosticsParams


/// <summary>
/// An alias for the type OmniSharp.Extensions.LanguageServer.Protocol.DocumentUri
/// </summary>
type UODocumentUri = OmniSharp.Extensions.LanguageServer.Protocol.DocumentUri

/// <summary>
/// An alias for the type OmniSharp.Extensions.LanguageServer.Protocol.Models.DiagnosticCode
/// </summary>
type UODiagnosticCode = OmniSharp.Extensions.LanguageServer.Protocol.Models.DiagnosticCode

/// <summary>
/// Prefix used to mark all trace log lines emitted by the FPL language server.
/// </summary>
let private tracePrefix = "######### "

/// <summary>
/// Recursively extracts the innermost exception message, prefixed with "; ".
/// </summary>
let rec private extractErrorMsg (ex: exn) =
    match ex.InnerException with
    | null -> "; " + ex.Message
    | inner -> extractErrorMsg inner

/// <summary>
/// Logs an informational trace message to the language client's Window channel.
/// </summary>
let private logMsg (languageServer: ILanguageServer) (message: string) (context: string) =
    languageServer.Window.LogInfo(tracePrefix + context + ": " + message)

/// <summary>
/// Logs an exception (innermost message only) to the language client's Window channel.
/// </summary>
let private logException (languageServer: ILanguageServer) (ex: exn) (context: string) =
    languageServer.Window.LogInfo(tracePrefix + context + ": " + (extractErrorMsg ex).Substring(2))

/// <summary>
/// Collects diagnostics grouped by their (escaped) source URI.
/// </summary>
type UriDiagnostics() =
    let diagnostics = Dictionary<PathEquivalentUri, List<UODiagnostic>>()

    /// <summary>
    /// Adds a diagnostic under the (escaped) key derived from the given URI.
    /// </summary>
    member _.AddDiagnostics(uri: PathEquivalentUri, diagnostic: UODiagnostic) =
        let key = PathEquivalentUri.EscapedUri(uri.AbsoluteUri)
        match diagnostics.TryGetValue(key) with
        | true, value -> value.Add(diagnostic)
        | false, _ ->
            let value = List<UODiagnostic>()
            value.Add(diagnostic)
            diagnostics.Add(key, value)

    /// <summary>
    /// Returns the underlying URI -> diagnostics dictionary.
    /// </summary>
    member _.Enumerator() = diagnostics

/// <summary>
/// Casts an FPL's internal Diagnostics severity into OmniSharp's DiagnosticSeverity.
/// </summary>
let private castSeverity (severity: DiagnosticSeverity) : UODiagnosticSeverity =
    if severity.IsError then
        UODiagnosticSeverity.Error
    elif severity.IsWarning then
        UODiagnosticSeverity.Warning
    elif severity.IsHint then
        UODiagnosticSeverity.Hint
    elif severity.IsInformation then
        UODiagnosticSeverity.Information
    else
        raise (NotImplementedException(severity.ToString()))

/// <summary>
/// Returns a message depending on the code of the diagnostic.
/// </summary>
let private castDiagnosticCodeMessage (diagnostic: Diagnostic) : string =
    diagnostic.Message

/// <summary>
/// Casts the error message depending on the emitter and severity.
/// </summary>
let private castMessage (diagnostic: Diagnostic) : string =
    castDiagnosticCodeMessage diagnostic

/// <summary>
/// Wraps a raw diagnostic code string into OmniSharp's DiagnosticCode.
/// </summary>
let private castCode (code: string) : UODiagnosticCode =
    UODiagnosticCode(code)

/// <summary>
/// Casts an F# ErrRecovery module diagnostic into OmniSharp's Diagnostic.
/// </summary>
/// <param name="diagnostic">Input diagnostic.</param>
/// <param name="tp">TextPositions object to handle ranges in the input stream.</param>
/// <returns>Casted diagnostic.</returns>
let castDiagnostic (diagnostic: Diagnostic) (tp: TextPositions) : UODiagnostic =
    UODiagnostic(
        Source = diagnostic.Emitter.ToString(),
        Severity = castSeverity diagnostic.Severity,
        Message = castMessage diagnostic,
        Range = tp.GetRange(int diagnostic.StartPos.Index, int diagnostic.EndPos.Index),
        Code = castCode diagnostic.Code.Code
    )

/// <summary>
/// Builds a dictionary mapping each parsed AST URI to a <see cref="TextPositions"/> instance
/// built from its source code.
/// </summary>
let private getTextPositionsByUri () : Dictionary<PathEquivalentUri, TextPositions> =
    let sourceCodes = heap.ParsedAsts.DictionaryOfSUri2FplSourceCode()
    let result = Dictionary<PathEquivalentUri, TextPositions>()
    sourceCodes
    |> Seq.iter (fun kvp -> result.Add(kvp.Key, TextPositions(kvp.Value)))
    result

/// <summary>
/// Handles publishing FPL parser/interpreter diagnostics to the language client.
/// </summary>
type DiagnosticsHandler(languageServer: ILanguageServer) =

    /// <summary>
    /// Casts a list of F# ErrRecovery module diagnostics into a list of OmniSharp's Diagnostics.
    /// </summary>
    /// <returns>Casted list.</returns>
    member _.CastDiagnostics() : UriDiagnostics =
        let castedListDiagnostics = UriDiagnostics()
        let uriTotextPositionsDict = getTextPositionsByUri ()
        logMsg languageServer diagnosticsContainer.DiagnosticsToString "~~~~~Diagnostics Count Orig"
        logMsg
            languageServer
            (uriTotextPositionsDict.Keys |> Seq.map (fun k -> k.AbsoluteUri) |> String.concat ", ")
            $"~~~~~{uriTotextPositionsDict.Keys.Count} source code keys"

        diagnosticsContainer.Collection
        |> Seq.iter (fun diagnostic ->
            let key = PathEquivalentUri.EscapedUri(diagnostic.Uri.AbsoluteUri)
            logMsg languageServer key.AbsoluteUri "~~~~~new key"
            logMsg languageServer diagnostic.Uri.AbsoluteUri "~~~~~old key"

            let tpByUri = uriTotextPositionsDict.[diagnostic.Uri]
            castedListDiagnostics.AddDiagnostics(diagnostic.Uri, castDiagnostic diagnostic tpByUri))

        logMsg languageServer (diagnosticsContainer.Collection.Length.ToString()) "~~~~~Diagnostics Count Orig"
        logMsg languageServer heap.ParsedAsts.TraceStatistics "~~~~~Statistics"

        castedListDiagnostics.Enumerator()
        |> Seq.iter (fun kvp ->
            logMsg languageServer $"{kvp.Value.Count} diagnostics in {kvp.Key.AbsolutePath}" "~~~~~Diagnostics Count VS Code")

        castedListDiagnostics

    /// <summary>
    /// Runs the FPL interpreter on the given buffer and refreshes the stored diagnostics.
    /// </summary>
    member this.RefreshFplDiagnosticsStorage(uri: PathEquivalentUri, buffer: StringBuilder) : UriDiagnostics =
        logMsg languageServer uri.AbsoluteUri "Uri in RefreshFplDiagnosticsStorage"
        if isNull (box buffer) then
            raise (ArgumentNullException(nameof buffer))

        let sourceCode = buffer.ToString()
        let fplLibUri = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        diagnosticsContainer.CurrentUri <- uri

        fplInterpreter sourceCode uri fplLibUri
        this.CastDiagnostics()

    /// <summary>
    /// Publishes diagnostics for the given URI/buffer to the language client, clearing
    /// diagnostics for any file that no longer reports any.
    /// </summary>
    member this.PublishDiagnostics(uri: PathEquivalentUri, buffer: StringBuilder) =
        if not (isNull (box buffer)) then
            try
                let allUris = HashSet<PathEquivalentUri>(heap.ParsedAsts |> Seq.map (fun pa -> pa.Parsing.Uri))
                let diagnostics = this.RefreshFplDiagnosticsStorage(uri, buffer)

                for diagnosticsPerUri in diagnostics.Enumerator() do
                    UOPublishDiagnosticsExtensions.PublishDiagnostics(
                        languageServer.TextDocument,
                        UOPublishDiagnosticsParams(
                            Uri = UODocumentUri.From(diagnosticsPerUri.Key),
                            Diagnostics = diagnosticsPerUri.Value
                        )
                    )
                    // remove Uri path from allUris because we have published them
                    allUris.Remove(diagnosticsPerUri.Key) |> ignore

                if diagnostics.Enumerator().Count = 0 then
                    UOPublishDiagnosticsExtensions.PublishDiagnostics(
                        languageServer.TextDocument,
                        UOPublishDiagnosticsParams(
                            Uri = UODocumentUri.From(uri),
                            Diagnostics = List<UODiagnostic>()
                        )
                    )

                // if they are still remaining allUris then publish empty diagnostics for them
                // this might happen if the user corrects the last error in the source code,
                // leaving it not emitting any more diagnostics. In this case we have to
                // delete the last language server diagnostics by explicitly publishing an empty diagnostics model list
                for uriPath in allUris do
                    UOPublishDiagnosticsExtensions.PublishDiagnostics(
                        languageServer.TextDocument,
                        UOPublishDiagnosticsParams(
                            Uri = UODocumentUri.From(uriPath), // reset remaining files
                            Diagnostics = List<UODiagnostic>()
                        )
                    )
                    logMsg languageServer uriPath.AbsoluteUri "Remaining in PublishDiagnostics"
            with ex ->
                logException languageServer ex "PublishDiagnostics"
        else
            logMsg languageServer "buffer was unexpectedly null" "PublishDiagnostics"
