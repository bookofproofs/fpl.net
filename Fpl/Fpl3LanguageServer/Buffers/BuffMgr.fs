module Fpl3LanguageServer.Buffers.BuffMgr
open System
open System.Text
open System.Collections.Concurrent
open Fpl0Base.Errors.Diagnostics

/// <summary>
/// Event args carrying the URI of a document whose buffer has been updated.
/// </summary>
type DocumentUpdatedEventArgs(uri: PathEquivalentUri) =
    inherit EventArgs()
    member _.Uri = uri

/// <summary>
/// Manages in-memory text buffers for open documents, keyed by their normalized URI.
/// </summary>
type BufferManager() =

    let buffers = ConcurrentDictionary<PathEquivalentUri, StringBuilder>()

    let bufferUpdated = new Event<EventHandler<DocumentUpdatedEventArgs>, DocumentUpdatedEventArgs>()

    [<CLIEvent>]
    member _.BufferUpdated = bufferUpdated.Publish

    member this.UpdateBuffer(uri: PathEquivalentUri, buffer: StringBuilder) =
        buffers.AddOrUpdate(uri, buffer, (fun _ _ -> buffer)) |> ignore
        bufferUpdated.Trigger(this, DocumentUpdatedEventArgs(uri))

    member _.GetBuffer(uri: PathEquivalentUri) : StringBuilder =
        match buffers.TryGetValue(uri) with
        | true, buffer -> buffer
        | false, _ -> null
