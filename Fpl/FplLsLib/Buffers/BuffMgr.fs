module FplLsLib.Buffers.BuffMgr
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
