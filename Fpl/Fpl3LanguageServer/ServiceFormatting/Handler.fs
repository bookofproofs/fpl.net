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
open Fpl3LanguageServer.Buffers.TextPos
open Fpl1Parser.Main
open System.Collections.Generic
open Fpl1Parser.Types
open Fpl1Parser.LSRelated.CommentLexer
open Fpl1Parser.LSRelated.Trivia
open Fpl1Parser.LSRelated.TriviaMap
open Fpl1Parser.LSRelated.FormattingOptions
open Fpl1Parser.LSRelated.PrettyPrint

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

                    // Entry point into PrettyPrint: printAll drives print recursively per node
                    let formattedText = printAll fplFormatDefaults triviaMap asts

                    // Wrap as a single full-document TextEdit
                    let textPositions = TextPositions(originalText)
                    let fullRange = textPositions.GetRange(0, originalText.Length)
                    return TextEditContainer([| TextEdit(Range = fullRange, NewText = formattedText) |])
            }

        member _.SetCapability(cap: DocumentFormattingCapability) =
            capability <- cap
