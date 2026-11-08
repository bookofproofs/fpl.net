/// <summary>
/// Language server handler for the textDocument/formatting request of FPL files.
/// </summary>
/// <remarks>
/// The handler parses the buffer, collects comments and node positions, builds a trivia map that
/// associates comments with nodes, and calls the FPL pretty-printer to produce the formatted text.
/// The result is returned as one full-document <c>TextEdit</c>.
/// </remarks>
module Fpl3LanguageServer.ServiceFormatting.FormattingHandler
open System.Threading
open System.Threading.Tasks
open OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities
open OmniSharp.Extensions.LanguageServer.Protocol.Document
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open OmniSharp.Extensions.LanguageServer.Protocol.Server
open Fpl0Base.Errors.Diagnostics
open Fpl3LanguageServer.Buffers.BuffMgr
open Fpl3LanguageServer.Buffers.Logging
open Fpl3LanguageServer.Buffers.TextPos
open Fpl3LanguageServer.Buffers.SettingsStore
open Fpl1Parser.Main
open System.Collections.Generic
open Fpl1Parser.Types
open Fpl1Parser.LSRelated.CommentLexer
open Fpl1Parser.LSRelated.Trivia
open Fpl1Parser.LSRelated.TriviaMap
open Fpl1Parser.LSRelated.PrettyPrint
open Fpl3LanguageServer.ServiceFormatting.SettingsPull

/// <summary>
/// Handles textDocument/formatting requests, delegating to the FPL pretty-printer
/// to compute a full-document text edit that reformats the buffer.
/// </summary>
/// <param name="languageServer">The language server instance used for logging and configuration requests.</param>
/// <param name="bufferManager">Provides the current text of open FPL buffers.</param>
/// <param name="settingsStore">Caches the most recent formatting options.</param>
/// <remarks>
/// The handler registers for files matching <c>**/*.fpl</c>.
/// </remarks>
type FormattingHandler(languageServer: ILanguageServer, bufferManager: BufferManager, settingsStore: SettingsStore) =

    /// <summary>
    /// Document selector that limits the registration to FPL source files.
    /// </summary>
    let documentSelector = DocumentSelector(DocumentFilter(Pattern = "**/*.fpl"))

    /// <summary>
    /// The client capability for document formatting, stored by <c>SetCapability</c>. The handler does not read it.
    /// </summary>
    let mutable capability = DocumentFormattingCapability()

    interface IDocumentFormattingHandler with

        /// <summary>
        /// Returns the registration options that tell the client which documents this handler supports.
        /// </summary>
        /// <returns>A <c>DocumentFormattingRegistrationOptions</c> with the FPL document selector.</returns>
        member _.GetRegistrationOptions() =
            DocumentFormattingRegistrationOptions(DocumentSelector = documentSelector)

        /// <summary>
        /// Handles a formatting request: parses the buffer, pretty-prints it with the client's current options,
        /// and returns one edit that replaces the whole document.
        /// </summary>
        /// <param name="request">The request with the document URI and the client's formatting options.</param>
        /// <param name="cancellationToken">Cancellation token of the request. The handler does not check it.</param>
        /// <returns>
        /// A task that yields a <c>TextEditContainer</c>. The container is empty if no buffer exists for the document.
        /// </returns>
        /// <remarks>
        /// The current formatting options are pulled from the client on every request through
        /// <c>pullCurrentOptionsAsync</c>. The result is also written to <c>settingsStore</c>.
        /// The indentation used by the pretty-printer comes from <c>printAll</c> and the pulled options. The
        /// <c>request.Options</c> values are not used for that.
        /// </remarks>
        member _.Handle(request: DocumentFormattingParams, cancellationToken: CancellationToken) : Task<TextEditContainer> =
            task {
                logMsg languageServer "Task<TextEditContainer>" "FormattingHandler.Handle"
                let uri = PathEquivalentUri(request.TextDocument.Uri.GetFileSystemPath())
                let buffer = bufferManager.GetBuffer(uri)
                if isNull buffer then
                    return TextEditContainer()
                else
                    let originalText = buffer.ToString()

                    // Parse (comment-stripped input, same pipeline the interpreter uses)
                    let asts, _ = fplParser originalText
                    // Discover comments independently, from raw source
                    let comments = findComments originalText

                    // Collect node positions from the parsed AST
                    let nodePositions =
                        asts
                        |> List.collect (fun a ->
                            let acc = List<Positions>()
                            collectPositions acc a
                            List.ofSeq acc)

                    // Merge comments + node positions into a lookup table
                    let triviaMap = buildTriviaMap nodePositions comments

                    // Pull the client's current formatting preferences on every request via a raw
                    // workspace/configuration request, rather than relying on a cached
                    // SettingsStore value (which is only ever updated by the unreliable
                    // workspace/didChangeConfiguration push path). Also refresh SettingsStore so
                    // other consumers see the latest value too.
                    let! options = pullCurrentOptionsAsync languageServer settingsStore.Current
                    settingsStore.UpdateOptions(options)

                    // Entry point into PrettyPrint: printAll drives print recursively per node
                    let formattedText = printAll options triviaMap asts

                    // Wrap as a single full-document TextEdit
                    let textPositions = TextPositions(originalText)
                    let fullRange = textPositions.GetRange(0, originalText.Length)
                    return TextEditContainer([| TextEdit(Range = fullRange, NewText = formattedText) |])
            }

        /// <summary>
        /// Stores the formatting capability that the client reported during initialization.
        /// </summary>
        /// <param name="cap">The client's <c>DocumentFormattingCapability</c>.</param>
        /// <remarks>
        /// The stored value is not used by <c>Handle</c>. Keep it only if other parts of the server need it.
        /// </remarks>
        member _.SetCapability(cap: DocumentFormattingCapability) =
            capability <- cap
