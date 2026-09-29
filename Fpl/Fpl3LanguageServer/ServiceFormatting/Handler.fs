module Fpl3LanguageServer.ServiceFormatting.Handler
open System.Threading
open System.Threading.Tasks
open OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities
open OmniSharp.Extensions.LanguageServer.Protocol.Document
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open OmniSharp.Extensions.LanguageServer.Protocol.Server
open Fpl0Base.Errors.Diagnostics
open Fpl3LanguageServer.Buffers.BuffMgr
open Fpl3LanguageServer.Buffers.Logging
open Fpl1Parser.Main
open Fpl1Parser.Formatting

/// <summary>
/// Handles textDocument/formatting requests, delegating to the FPL pretty-printer
/// to compute a full-document text edit that reformats the buffer.
/// </summary>
type FormattingHandler(languageServer: ILanguageServer, bufferManager: BufferManager) =

    let documentSelector = DocumentSelector(DocumentFilter(Pattern = "**/*.fpl"))

    let mutable capability = DocumentFormattingCapability()

    interface IDocumentFormattingHandler with

        member _.GetRegistrationOptions() =
            DocumentFormattingRegistrationOptions(DocumentSelector = documentSelector)

        member _.Handle(request: DocumentFormattingParams, cancellationToken: CancellationToken) : Task<TextEditContainer> =
            task {
                logMsg languageServer "Task<TextEditContainer>" "FormattingHandler.Handle"
                let uri = PathEquivalentUri(request.TextDocument.Uri.GetFileSystemPath())
                let buffer = bufferManager.GetBuffer(uri)
                if isNull buffer then
                    return TextEditContainer()
                else
                    let originalText = buffer.ToString()
                    let indentSize =
                        if request.Options.InsertSpaces then int request.Options.TabSize else 4
                    let asts, _wasFullyParsed = fplParser originalText
                    let formattedText = prettyPrint indentSize asts
                    let lines = originalText.Split('\n')
                    let lastLine = lines.Length - 1
                    let lastCol = lines.[lastLine].TrimEnd('\r').Length
                    let fullRange =
                        Range(Position(0, 0), Position(lastLine, lastCol))
                    return TextEditContainer([| TextEdit(Range = fullRange, NewText = formattedText) |])
            }

        member _.SetCapability(cap: DocumentFormattingCapability) =
            capability <- cap
